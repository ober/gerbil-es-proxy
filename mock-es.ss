(export #t)
(import :std/net/httpd
        :std/text/json
        :std/misc/uuid
        :std/format
        :std/sugar
        :std/srfi/19)

;; Thread-safe atomic counter for request tracking
(defstruct atomic-counter (value mutex)
  transparent: #t)

(def (new-atomic-counter (init 0))
  (make-atomic-counter init (make-mutex)))

(def (atomic-counter-increment! ac)
  (let (mx (atomic-counter-mutex ac))
    (mutex-lock! mx)
    (unwind-protect
     (let (v (+ 1 (atomic-counter-value ac)))
       (set! (atomic-counter-value ac) v)
       v)
     (mutex-unlock! mx))))

(def (atomic-counter-get ac)
  (let (mx (atomic-counter-mutex ac))
    (mutex-lock! mx)
    (unwind-protect
     (atomic-counter-value ac)
     (mutex-unlock! mx))))

;; JSON helper: serialize a hash table to string
(def (json->string val)
  (call-with-output-string
   (lambda (p)
     (parameterize ((current-output-port p))
       (write-json val)))))

;; JSON helper: parse a string to JSON
(def (string->json str)
  (call-with-input-string str read-json))

;; Generate a timestamp in RFC 3339 format
(def (rfc3339-now)
  (date->string (current-date) "~Y-~m-~dT~H:~M:~SZ"))

;; Mock Elasticsearch server
(defstruct mock-es (port server request-counter)
  transparent: #t)

(def (make-mock-es-server (port 19200))
  (let* ((counter (new-atomic-counter 0))
         (mux (make-default-http-mux
               (lambda (req res)
                 (atomic-counter-increment! counter)
                 (handle-mock-request req res))))
         (srv #f))
    (make-mock-es port srv counter)))

(def (handle-mock-request req res)
  (let* ((method (http-request-method req))
         (path (http-request-path req))
         (body-bytes (http-request-body req))
         (body (if (and body-bytes (u8vector? body-bytes))
                 (bytes->string body-bytes)
                 "")))
    (cond
     ;; GET /
     ((and (eq? method 'GET) (equal? path "/"))
      (respond-json res 200
        (hash ("name" "mock-es-node")
              ("cluster_name" "mock-cluster")
              ("cluster_uuid" "abc123")
              ("version" (hash ("number" "8.0.0")))
              ("tagline" "You Know, for Search"))))

     ;; GET /_cluster/health
     ((and (eq? method 'GET) (equal? path "/_cluster/health"))
      (respond-json res 200
        (hash ("cluster_name" "mock-cluster")
              ("status" "green")
              ("number_of_nodes" 3)
              ("active_shards" 100))))

     ;; GET /_cat/indices
     ((and (eq? method 'GET) (equal? path "/_cat/indices"))
      (http-response-write res 200
        '(("Content-Type" . "text/plain"))
        "green open logs-2024.01 5 1 1000 0 10mb 5mb\n"))

     ;; POST requests
     ((eq? method 'POST)
      (handle-post req res path body))

     ;; PUT requests
     ((eq? method 'PUT)
      (handle-put req res path body))

     ;; DELETE requests
     ((eq? method 'DELETE)
      (respond-json res 200 (hash ("acknowledged" #t))))

     ;; HEAD requests
     ((eq? method 'HEAD)
      (http-response-write res 200 '() #f))

     ;; Catch-all
     (else
      (respond-json res 200 (hash))))))

(def (respond-json res status data)
  (http-response-write res status
    '(("Content-Type" . "application/json"))
    (json->string data)))

(def (handle-post req res path body)
  (cond
   ;; /_search endpoints
   ((string-suffix? "/_search" path)
    (handle-search res path body))
   ;; /_bulk endpoint
   ((string-suffix? "/_bulk" path)
    (handle-bulk res body))
   ;; /_doc endpoint
   ((string-contains path "/_doc")
    (handle-index-document res path body))
   ;; Generic POST
   (else
    (respond-json res 200 (hash ("acknowledged" #t))))))

(def (handle-search res path body)
  (let* ((parts (filter (lambda (s) (> (string-length s) 0))
                        (string-split path #\/)))
         (index (if (pair? parts) (car parts) "unknown"))
         (size 10)
         (from 0))
    ;; Parse search body for size/from
    (when (> (string-length body) 0)
      (with-catch
       (lambda (e) #f)  ;; ignore parse errors
       (lambda ()
         (let (json (string->json body))
           (when (hash-table? json)
             (let (s (hash-get json "size"))
               (when s (set! size (min s 10))))
             (let (f (hash-get json "from"))
               (when f (set! from f))))))))
    ;; Generate hits
    (let* ((index-resolved (string-map (lambda (c) (if (char=? c #\*) #\2 c)) index))
           ;; fix wildcard -> real index name
           (index-name (if (string-contains index "*")
                         (string-append (substring index 0 (string-index index #\*)) "2024.01")
                         index))
           (hits
            (let loop ((i 0) (acc '()))
              (if (>= i (min size 10))
                (reverse acc)
                (loop (+ i 1)
                      (cons (hash ("_index" index-name)
                                  ("_id" (number->string (+ i from)))
                                  ("_score" (- 1.0 (* i 0.1)))
                                  ("_source" (hash ("message" (format "Log entry ~a" (+ i from)))
                                                   ("@timestamp" (rfc3339-now)))))
                            acc))))))
      (respond-json res 200
        (hash ("took" 5)
              ("timed_out" #f)
              ("_shards" (hash ("total" 5) ("successful" 5) ("skipped" 0) ("failed" 0)))
              ("hits" (hash ("total" (hash ("value" 100) ("relation" "eq")))
                            ("max_score" 1.0)
                            ("hits" hits))))))))

(def (handle-bulk res body)
  (let* ((lines (filter (lambda (s) (> (string-length s) 0))
                        (string-split body #\newline)))
         (items '())
         (errors #f))
    ;; Process pairs of action/document lines
    (let loop ((rest lines))
      (when (>= (length rest) 2)
        (let ((action-line (car rest))
              (doc-line (cadr rest)))
          (with-catch
           (lambda (e)
             (set! errors #t)
             (set! items
               (cons (hash ("index"
                            (hash ("_index" "unknown")
                                  ("_id" "error")
                                  ("status" 400)
                                  ("error" (hash ("type" "parse_exception")
                                                 ("reason" (error-message e)))))))
                     items)))
           (lambda ()
             (let* ((action-json (string->json action-line))
                    (action-type (car (hash-keys action-json)))
                    (action-meta (hash-ref action-json action-type))
                    (index (or (hash-get action-meta "_index")
                               (hash-get action-meta "index")
                               "unknown"))
                    (doc-id (or (hash-get action-meta "_id")
                                (hash-get action-meta "id")
                                (uuid->string (random-uuid)))))
               ;; Validate doc is valid JSON
               (string->json doc-line)
               (let (item-hash (make-hash-table))
                 (hash-put! item-hash action-type
                   (hash ("_index" index)
                         ("_id" doc-id)
                         ("_version" 1)
                         ("result" "created")
                         ("_shards" (hash ("total" 2) ("successful" 1) ("failed" 0)))
                         ("status" 201)
                         ("_seq_no" 0)
                         ("_primary_term" 1)))
                 (set! items (cons item-hash items))))))
          (loop (cddr rest)))))
    (respond-json res 200
      (hash ("took" (* (length items) 3))
            ("errors" (if errors #t #f))
            ("items" (reverse items))))))

(def (handle-index-document res path body)
  (let* ((parts (filter (lambda (s) (> (string-length s) 0))
                        (string-split path #\/)))
         (index (if (pair? parts) (car parts) "unknown"))
         (doc-id (if (>= (length parts) 3)
                   (list-ref parts 2)
                   (uuid->string (random-uuid)))))
    (with-catch
     (lambda (e)
       (respond-json res 400
         (hash ("error" (hash ("root_cause" [(hash ("type" "parse_exception")
                                                   ("reason" (format "Failed to parse: ~a" (error-message e))))])
                               ("type" "parse_exception")
                               ("reason" (format "Failed to parse: ~a" (error-message e)))))
               ("status" 400))))
     (lambda ()
       (when (> (string-length body) 0)
         (string->json body))
       (respond-json res 201
         (hash ("_index" index)
               ("_id" doc-id)
               ("_version" 1)
               ("result" "created")
               ("_shards" (hash ("total" 2) ("successful" 1) ("failed" 0)))
               ("_seq_no" 0)
               ("_primary_term" 1)))))))

(def (handle-put req res path body)
  (let* ((parts (filter (lambda (s) (> (string-length s) 0))
                        (string-split path #\/)))
         (index (if (pair? parts) (car parts) "unknown")))
    (cond
     ;; Mapping or settings update
     ((or (string-suffix? "/_mapping" path) (string-suffix? "/_settings" path))
      (with-catch
       (lambda (e)
         (respond-json res 400
           (hash ("error" (hash ("type" "parse_exception")
                                ("reason" (error-message e))))
                 ("status" 400))))
       (lambda ()
         (when (> (string-length body) 0)
           (string->json body))
         (respond-json res 200 (hash ("acknowledged" #t))))))
     ;; Create index
     (else
      (respond-json res 200
        (hash ("acknowledged" #t)
              ("shards_acknowledged" #t)
              ("index" index)))))))

;; Start mock ES server
(def (mock-es-start! mock)
  (let* ((addr (format "127.0.0.1:~a" (mock-es-port mock)))
         (mux (make-default-http-mux
               (lambda (req res)
                 (atomic-counter-increment! (mock-es-request-counter mock))
                 (handle-mock-request req res))))
         (srv (start-http-server! mux: mux addr)))
    (set! (mock-es-server mock) srv)
    (thread-sleep! 0.1)
    mock))

;; Stop mock ES server
(def (mock-es-stop! mock)
  (when (mock-es-server mock)
    (stop-http-server! (mock-es-server mock))
    (set! (mock-es-server mock) #f)))

;; Get request count
(def (mock-es-request-count mock)
  (atomic-counter-get (mock-es-request-counter mock)))
