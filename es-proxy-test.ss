(export es-proxy-test)
(import :std/test
        :std/net/request
        :std/text/json
        :std/format
        :std/sugar
        :std/misc/uuid
        :std/srfi/19
        (only-in :std/net/httpd start-http-server! stop-http-server!
                 make-default-http-mux http-request-method http-request-path
                 http-request-body http-request-headers http-response-write)
        :es-proxy/mock-es
        :es-proxy/es-proxy)

;; Helper: parse JSON from response
(def (response-json resp)
  (call-with-input-string (request-text resp) read-json))

;; Helper: wait for server to be ready
(def (wait-for-server host port (attempts 20))
  (let loop ((n 0))
    (if (>= n attempts)
      (error "Server did not start" host port)
      (with-catch
       (lambda (e) (thread-sleep! 0.1) (loop (+ n 1)))
       (lambda ()
         (let (resp (http-get (format "http://~a:~a/" host port)))
           (request-close resp)
           #t))))))

(def es-proxy-test
  (test-suite "ES Proxy Tests"

    ;; Test 1: Search response validation
    (test-case "validates search response matches request parameters"
      (let ((mock (make-mock-es-server 19210)))
        (mock-es-start! mock)
        (with-unwind-protect
         (lambda ()
           (let* ((search-body "{\"query\":{\"match\":{\"field\":\"value\"}},\"size\":5,\"from\":10}")
                  (resp (http-post "http://127.0.0.1:19210/logs-2024.01/_search"
                          headers: '(("Content-Type" . "application/json"))
                          data: search-body))
                  (status (request-status resp))
                  (json (response-json resp)))
             (request-close resp)
             (check status => 200)
             ;; Validate response structure
             (check (hash-ref json "took") => 5)
             (check (hash-ref json "timed_out") => #f)
             (let (shards (hash-ref json "_shards"))
               (check (hash-ref shards "total") => 5)
               (check (hash-ref shards "successful") => 5)
               (check (hash-ref shards "failed") => 0))
             ;; Validate hits
             (let (hits (hash-ref json "hits"))
               (check (hash-ref (hash-ref hits "total") "value") => 100)
               (check (hash-ref hits "max_score") => 1.0)
               ;; Validate returned hits match requested size
               (let (hit-list (hash-ref hits "hits"))
                 (check (length hit-list) => 5)
                 ;; Each hit has correct index
                 (for-each
                  (lambda (hit)
                    (check (string-contains (hash-ref hit "_index") "logs") ? values)
                    (check (string-length (hash-ref (hash-ref hit "_source") "message")) ? positive?))
                  hit-list)))))
         (lambda () (mock-es-stop! mock)))))

    ;; Test 2: Bulk response validation
    (test-case "validates bulk response matches number of documents sent"
      (let ((mock (make-mock-es-server 19211)))
        (mock-es-start! mock)
        (with-unwind-protect
         (lambda ()
           (let* ((bulk-body (string-append
                              "{\"index\":{\"_index\":\"logs-2024.01\",\"_id\":\"doc1\"}}\n"
                              "{\"message\":\"First log\",\"level\":\"INFO\"}\n"
                              "{\"index\":{\"_index\":\"logs-2024.01\",\"_id\":\"doc2\"}}\n"
                              "{\"message\":\"Second log\",\"level\":\"WARN\"}\n"
                              "{\"index\":{\"_index\":\"logs-2024.01\"}}\n"
                              "{\"message\":\"Third log\",\"level\":\"ERROR\"}\n"))
                  (resp (http-post "http://127.0.0.1:19211/_bulk"
                          headers: '(("Content-Type" . "application/x-ndjson"))
                          data: bulk-body))
                  (json (response-json resp)))
             (request-close resp)
             ;; Validate response structure
             (check (hash-ref json "errors") => #f)
             (let (items (hash-ref json "items"))
               (check (length items) => 3)
               ;; Each item created successfully
               (for-each
                (lambda (item)
                  (let (idx-resp (hash-ref item "index"))
                    (check (hash-ref idx-resp "_index") => "logs-2024.01")
                    (check (hash-ref idx-resp "status") => 201)
                    (check (hash-ref idx-resp "result") => "created")))
                items)
               ;; Verify specific IDs preserved
               (check (hash-ref (hash-ref (list-ref items 0) "index") "_id") => "doc1")
               (check (hash-ref (hash-ref (list-ref items 1) "index") "_id") => "doc2")
               ;; Third doc has auto-generated ID
               (check (string-length (hash-ref (hash-ref (list-ref items 2) "index") "_id")) ? positive?))))
         (lambda () (mock-es-stop! mock)))))

    ;; Test 3: Index document validation
    (test-case "validates index document response includes correct index and generates ID"
      (let ((mock (make-mock-es-server 19212)))
        (mock-es-start! mock)
        (with-unwind-protect
         (lambda ()
           (let* ((doc-body "{\"message\":\"Test document\",\"@timestamp\":\"2024-01-15T10:30:00Z\",\"level\":\"INFO\"}")
                  (resp (http-post "http://127.0.0.1:19212/my-index/_doc"
                          headers: '(("Content-Type" . "application/json"))
                          data: doc-body))
                  (json (response-json resp)))
             (request-close resp)
             (check (request-status resp) => 201)
             (check (hash-ref json "_index") => "my-index")
             (check (string-length (hash-ref json "_id")) ? positive?)
             (check (hash-ref json "_version") => 1)
             (check (hash-ref json "result") => "created")
             (check (hash-ref (hash-ref json "_shards") "successful") ? positive?)
             ;; Test with explicit ID
             (let* ((resp2 (http-post "http://127.0.0.1:19212/my-index/_doc/explicit-id-123"
                             headers: '(("Content-Type" . "application/json"))
                             data: doc-body))
                    (json2 (response-json resp2)))
               (request-close resp2)
               (check (hash-ref json2 "_id") => "explicit-id-123"))))
         (lambda () (mock-es-stop! mock)))))

    ;; Test 4: Error response for invalid JSON
    (test-case "validates error response for invalid JSON"
      (let ((mock (make-mock-es-server 19213)))
        (mock-es-start! mock)
        (with-unwind-protect
         (lambda ()
           (let* ((resp (http-post "http://127.0.0.1:19213/logs/_doc"
                          headers: '(("Content-Type" . "application/json"))
                          data: "{invalid json"))
                  (json (response-json resp)))
             (request-close resp)
             (check (request-status resp) => 400)
             (check (hash-ref (hash-ref json "error") "type") => "parse_exception")
             (check (hash-ref json "status") => 400)))
         (lambda () (mock-es-stop! mock)))))

    ;; Test 5: PUT index creation
    (test-case "validates PUT index creation response"
      (let ((mock (make-mock-es-server 19214)))
        (mock-es-start! mock)
        (with-unwind-protect
         (lambda ()
           (let* ((settings-body "{\"settings\":{\"number_of_shards\":5,\"number_of_replicas\":1}}")
                  (resp (http-put "http://127.0.0.1:19214/new-index"
                          headers: '(("Content-Type" . "application/json"))
                          data: settings-body))
                  (json (response-json resp)))
             (request-close resp)
             (check (request-status resp) => 200)
             (check (hash-ref json "acknowledged") => #t)
             (check (hash-ref json "shards_acknowledged") => #t)
             (check (hash-ref json "index") => "new-index")))
         (lambda () (mock-es-stop! mock)))))

    ;; Test 6: Proxy through es-proxy
    (test-case "proxies traffic through es-proxy to mock ES"
      (let ((mock (make-mock-es-server 19215))
            (proxy-port 19216))
        (mock-es-start! mock)
        (with-unwind-protect
         (lambda ()
           ;; Start es-proxy in plain HTTP mode pointing at mock ES
           (let ((srv (parameterize ((current-remote-host "127.0.0.1")
                                     (current-remote-port 19215)
                                     (current-use-tls #f))
                        (start-es-proxy "127.0.0.1" proxy-port))))
             (with-unwind-protect
              (lambda ()
                (wait-for-server "127.0.0.1" proxy-port)

                ;; Test cluster info via es-proxy
                (let* ((resp (http-get (format "http://127.0.0.1:~a/" proxy-port)))
                       (body (request-text resp)))
                  (request-close resp)
                  (check (request-status resp) => 200)
                  (check (string-contains body "mock-cluster") ? values))

                ;; Test search via es-proxy
                (let* ((resp (http-post (format "http://127.0.0.1:~a/logs/_search" proxy-port)
                               headers: '(("Content-Type" . "application/json"))
                               data: "{\"query\":{\"match_all\":{}}}"))
                       (body (request-text resp)))
                  (request-close resp)
                  (check (request-status resp) => 200)
                  (check (string-contains body "hits") ? values))

                ;; Test bulk via es-proxy
                (let* ((bulk-body (string-append
                                   "{\"index\":{\"_index\":\"logs\"}}\n"
                                   "{\"message\":\"test\"}\n"))
                       (resp (http-post (format "http://127.0.0.1:~a/_bulk" proxy-port)
                               headers: '(("Content-Type" . "application/x-ndjson"))
                               data: bulk-body))
                       (body (request-text resp)))
                  (request-close resp)
                  (check (request-status resp) => 200)
                  (check (string-contains body "items") ? values))

                ;; Test cluster health via es-proxy
                (let* ((resp (http-get (format "http://127.0.0.1:~a/_cluster/health" proxy-port)))
                       (body (request-text resp)))
                  (request-close resp)
                  (check (request-status resp) => 200)
                  (check (string-contains body "green") ? values))

                ;; Verify mock ES received all requests
                (check (mock-es-request-count mock) ? (cut >= <> 4)))
              (lambda () (stop-http-server! srv)))))
         (lambda () (mock-es-stop! mock)))))

    ;; Test 7: High volume traffic
    (test-case "handles high volume traffic"
      (let ((mock (make-mock-es-server 19217)))
        (mock-es-start! mock)
        (with-unwind-protect
         (lambda ()
           (let ((request-count 100)
                 (completed (new-atomic-counter 0)))
             ;; 10 concurrent client threads
             (let (threads
                   (map (lambda (i)
                          (spawn
                           (lambda ()
                             (let (client-url "http://127.0.0.1:19217/_cluster/health")
                               (let loop ((n 0))
                                 (when (< n 10)
                                   (let (resp (http-get client-url))
                                     (request-close resp)
                                     (atomic-counter-increment! completed)
                                     (loop (+ n 1)))))))))
                        (iota 10)))
               ;; Wait for all threads to finish
               (for-each thread-join! threads)
               (check (atomic-counter-get completed) => request-count)
               (check (mock-es-request-count mock) => request-count))))
         (lambda () (mock-es-stop! mock)))))

    ;; Test 8: Memory leak detection
    (test-case "detects memory leaks under sustained load"
      (let ((mock (make-mock-es-server 19218)))
        (mock-es-start! mock)
        (with-unwind-protect
         (lambda ()
           ;; Force GC before baseline
           (##gc)
           (thread-sleep! 0.5)
           ;; Warm up — establish baseline after initial allocations
           (let* ((warmup-threads
                   (map (lambda (i)
                          (spawn
                           (lambda ()
                             (let loop ((n 0))
                               (when (< n 20)
                                 (let (resp (http-get "http://127.0.0.1:19218/_cluster/health"))
                                   (request-close resp)
                                   (loop (+ n 1))))))))
                        (iota 5))))
             (for-each thread-join! warmup-threads))
           (##gc)
           (thread-sleep! 0.5)
           (let* ((baseline-stats (##process-statistics))
                  (baseline-heap (f64vector-ref baseline-stats 17)))
             (displayln "\n  Baseline heap: " baseline-heap)
             (let ((iterations 5)
                   (requests-per-iteration 200)
                   (snapshots '()))
               ;; Run sustained load
               (let iloop ((iter 0))
                 (when (< iter iterations)
                   (let* ((completed (new-atomic-counter 0))
                          (threads
                           (map (lambda (i)
                                  (spawn
                                   (lambda ()
                                     (let loop ((n 0))
                                       (when (< n 20)
                                         (let (resp
                                               (case (modulo n 4)
                                                 ((0) (http-get "http://127.0.0.1:19218/_cluster/health"))
                                                 ((1) (http-post "http://127.0.0.1:19218/logs/_search"
                                                        headers: '(("Content-Type" . "application/json"))
                                                        data: "{\"query\":{\"match_all\":{}}}"))
                                                 ((2) (http-post "http://127.0.0.1:19218/_bulk"
                                                        headers: '(("Content-Type" . "application/x-ndjson"))
                                                        data: "{\"index\":{\"_index\":\"logs\"}}\n{\"msg\":\"test\"}\n"))
                                                 (else (http-get "http://127.0.0.1:19218/_cat/indices"))))
                                           (request-close resp)
                                           (atomic-counter-increment! completed)
                                           (loop (+ n 1))))))))
                                (iota 10))))
                     (for-each thread-join! threads)
                     ;; GC and snapshot
                     (##gc)
                     (thread-sleep! 0.3)
                     (let* ((stats (##process-statistics))
                            (heap (f64vector-ref stats 17)))
                       (set! snapshots (append snapshots (list heap)))
                       (displayln "  Iteration " (+ iter 1) ": heap=" heap
                                  " growth=" (- heap baseline-heap))))
                   (iloop (+ iter 1))))
               ;; Analyze: final growth shouldn't be much larger than midpoint
               ;; Allow generous tolerance for GC-managed heap
               (let* ((mid-growth (- (list-ref snapshots (quotient iterations 2)) baseline-heap))
                      (final-growth (- (list-ref snapshots (- iterations 1)) baseline-heap))
                      (stabilized (or (<= final-growth 0)
                                      (<= final-growth (+ (* mid-growth 2.0) 1.0)))))
                 (displayln "  Final growth: " final-growth " Mid growth: " mid-growth)
                 (displayln "  Memory stabilized: " stabilized)
                 ;; Should not grow unbounded
                 (check stabilized ? values)))))
         (lambda () (mock-es-stop! mock)))))))
