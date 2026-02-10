(export es-proxy-stress-test)
(import :std/test
        :std/net/request
        :std/text/json
        :std/format
        :std/sugar
        :std/misc/uuid
        :std/srfi/19
        (only-in :std/net/httpd start-http-server! stop-http-server!
                 make-default-http-mux)
        :es-proxy/mock-es)

(def es-proxy-stress-test
  (test-suite "ES Proxy Stress Tests"

    ;; Test 9: Extended stress test with sustained high load
    (test-case "stress tests with sustained high load for extended duration"
      (let ((mock (make-mock-es-server 19219)))
        (mock-es-start! mock)
        (with-unwind-protect
         (lambda ()
           (let* ((duration-seconds 300)
                  (concurrent-clients 50)
                  (completed (new-atomic-counter 0))
                  (errors (new-atomic-counter 0))
                  (start-time (exact->inexact (time->seconds (##current-time)))))

             (displayln (format "\n  Starting ~as stress test with ~a concurrent clients..."
                                duration-seconds concurrent-clients))

             ;; Force GC before measuring baseline
             (##gc)
             (thread-sleep! 0.1)
             (let ((baseline-stats (##process-statistics))
                   (baseline-heap (f64vector-ref (##process-statistics) 0)))
               (displayln "  Baseline heap: " baseline-heap)

               ;; Spawn concurrent clients that run for the full duration
               (let ((client-threads
                      (map (lambda (client-id)
                             (spawn
                              (lambda ()
                                (let loop ()
                                  (let ((elapsed (- (exact->inexact (time->seconds (##current-time)))
                                                    start-time)))
                                    (when (< elapsed duration-seconds)
                                      (with-catch
                                       (lambda (e)
                                         (atomic-counter-increment! errors))
                                       (lambda ()
                                         (case (random-integer 10)
                                           ;; Search with varying sizes (30%)
                                           ((0 1 2)
                                            (let* ((size (+ 1 (random-integer 100)))
                                                   (body (format "{\"query\":{\"bool\":{\"must\":[{\"match\":{\"level\":\"INFO\"}},{\"range\":{\"@timestamp\":{\"gte\":\"now-1h\"}}}]}},\"size\":~a,\"from\":~a,\"sort\":[{\"@timestamp\":\"desc\"}]}"
                                                                 size (random-integer 1000))))
                                              (let (resp (http-post "http://127.0.0.1:19219/logs-*/_search"
                                                           headers: '(("Content-Type" . "application/json"))
                                                           data: body))
                                                (request-close resp))))

                                           ;; Bulk indexing with varying batch sizes (30%)
                                           ((3 4 5)
                                            (let* ((batch-size (+ 10 (random-integer 50)))
                                                   (docs (let dloop ((i 0) (acc ""))
                                                           (if (>= i batch-size)
                                                             acc
                                                             (dloop (+ i 1)
                                                                    (string-append acc
                                                                      (format "{\"index\":{\"_index\":\"logs-2024.01\"}}\n{\"@timestamp\":\"~a\",\"message\":\"~a\",\"level\":\"INFO\",\"client_id\":~a,\"seq\":~a}\n"
                                                                              (date->string (current-date) "~Y-~m-~dT~H:~M:~SZ")
                                                                              (make-string (random-integer 500) #\x)
                                                                              client-id i)))))))
                                              (let (resp (http-post "http://127.0.0.1:19219/_bulk"
                                                           headers: '(("Content-Type" . "application/x-ndjson"))
                                                           data: docs))
                                                (request-close resp))))

                                           ;; Single document with large payload (20%)
                                           ((6 7)
                                            (let* ((payload-size (+ 1000 (random-integer 10000)))
                                                   (body (format "{\"@timestamp\":\"~a\",\"message\":\"~a\",\"metadata\":{\"client\":~a,\"random\":~a}}"
                                                                 (date->string (current-date) "~Y-~m-~dT~H:~M:~SZ")
                                                                 (make-string payload-size #\y)
                                                                 client-id (random-integer 1000000))))
                                              (let (resp (http-post "http://127.0.0.1:19219/logs-2024.01/_doc"
                                                           headers: '(("Content-Type" . "application/json"))
                                                           data: body))
                                                (request-close resp))))

                                           ;; Cluster operations (10%)
                                           ((8)
                                            (let (resp (http-get "http://127.0.0.1:19219/_cluster/health"))
                                              (request-close resp)))

                                           ;; Cat APIs (10%)
                                           (else
                                            (let (resp (http-get "http://127.0.0.1:19219/_cat/indices"))
                                              (request-close resp))))
                                         (atomic-counter-increment! completed)))
                                      (loop)))))))
                           (iota concurrent-clients))))

                 ;; Monitor progress and memory while test runs
                 (let ((snapshots '())
                       (last-completed 0))
                   (let mloop ()
                     (let ((elapsed (- (exact->inexact (time->seconds (##current-time))) start-time)))
                       (when (< elapsed duration-seconds)
                         (thread-sleep! 5)
                         (let* ((current (atomic-counter-get completed))
                                (rps (/ (- current last-completed) 5.0)))
                           (set! last-completed current)
                           (##gc)
                           (let* ((stats (##process-statistics))
                                  (heap (f64vector-ref stats 0))
                                  (growth (- heap baseline-heap)))
                             (set! snapshots (append snapshots (list heap)))
                             (displayln (format "  [~as] Requests: ~a, RPS: ~a, Errors: ~a, Heap growth: ~a"
                                                (inexact->exact (floor elapsed))
                                                current
                                                (inexact->exact (floor rps))
                                                (atomic-counter-get errors)
                                                growth))))
                         (mloop))))

                   ;; Wait for all clients to finish
                   (thread-sleep! 1)

                   ;; Final stats
                   (let* ((total-requests (atomic-counter-get completed))
                          (total-errors (atomic-counter-get errors))
                          (actual-duration (- (exact->inexact (time->seconds (##current-time))) start-time))
                          (final-rps (if (> actual-duration 0) (/ total-requests actual-duration) 0)))

                     (##gc)
                     (thread-sleep! 0.1)
                     (let* ((final-stats (##process-statistics))
                            (final-heap (f64vector-ref final-stats 0))
                            (final-growth (- final-heap baseline-heap))
                            (error-rate (if (> (+ total-requests total-errors) 0)
                                          (* 100.0 (/ (exact->inexact total-errors)
                                                      (+ total-requests total-errors)))
                                          0.0)))

                       (displayln "\n  === Stress Test Results ===")
                       (displayln (format "  Duration: ~as" (inexact->exact (floor actual-duration))))
                       (displayln (format "  Total requests: ~a" total-requests))
                       (displayln (format "  Total errors: ~a" total-errors))
                       (displayln (format "  Average RPS: ~a" (inexact->exact (floor final-rps))))
                       (displayln (format "  Error rate: ~a%" error-rate))
                       (displayln (format "  Final heap: ~a" final-heap))
                       (displayln (format "  Heap growth: ~a" final-growth))

                       ;; Assertions
                       (check (mock-es-request-count mock) => total-requests)
                       ;; Should complete significant work
                       (check (> total-requests 1000) ? values)
                       ;; Less than 1% error rate
                       (check (< (/ (exact->inexact total-errors)
                                    (max 1 (+ total-requests total-errors)))
                                 0.01) ? values)

                       ;; Memory should not grow unboundedly
                       (when (>= (length snapshots) 4)
                         (let* ((mid-heap (list-ref snapshots (quotient (length snapshots) 2)))
                                (mid-growth (- mid-heap baseline-heap))
                                (stabilized (<= final-growth (+ (* mid-growth 2.0) 2.0))))
                           (when (and (> final-growth 50.0) (not stabilized))
                             (error "Memory may be leaking: grew without stabilizing"
                                    final-growth)))))))))))
         (lambda () (mock-es-stop! mock)))))))
