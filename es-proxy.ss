(export #t)
(import :std/net/httpd
        :std/net/request
        :std/net/ssl
        :std/misc/uuid
        :std/text/base64
        :std/text/json
        :std/srfi/19
        :std/cli/getopt
        :std/sugar
        :std/format)

(def VERSION "0.2.0")

;; Runtime configuration parameters
(def current-remote-host (make-parameter "localhost"))
(def current-remote-port (make-parameter 9200))
(def current-use-tls (make-parameter #t))
(def current-cert-path (make-parameter "/usr/local/tls/cert_wildcard.pem"))
(def current-key-path (make-parameter "/usr/local/tls/keys/cert_wildcard_key.pem"))

;; Extract username from Basic Authorization header
(def (get-user-from-basic headers)
  (let (auth (assoc "Authorization" headers))
    (if (and auth (string-prefix? "Basic " (cdr auth)))
      (let* ((decoded (base64-decode (substring (cdr auth) 6 (string-length (cdr auth)))))
             (parts (string-split decoded #\:)))
        (if (pair? parts) (car parts) "anonymous"))
      "anonymous")))

;; Base64-encode a request body for logging
(def (print-body body)
  (if (and body (u8vector? body) (> (u8vector-length body) 0))
    (u8vector->base64-string body)
    ""))

;; Current timestamp with nanosecond precision
(def (current-timestamp)
  (let (d (current-date))
    (string-append
     (date->string d "~Y-~m-~d ~H:~M:~S")
     "."
     (number->string (date-nanosecond d)))))

;; Log a request in the es-proxy format
(def (log-request id user path params method source agent body)
  (let ((query (or params "None")))
    (displayln
     (format "~a ID: ~a User: ~a Path: ~a Query: ~a Method: ~a Source: ~a  Agent: ~a Body: ~a"
             (current-timestamp) id user path query method source agent (print-body body)))))

;; Log a response in the es-proxy format
(def (log-response id user path params method source agent status duration size)
  (let ((query (or params "None")))
    (displayln
     (format "~a ID: ~a User: ~a Path: ~a Query: ~a Method: ~a Source: ~a Agent: ~a Status: ~a Duration: ~as Size: ~a"
             (current-timestamp) id user path query method source agent status duration size))))

;; Extract source IP from client address
;; httpd client address is a pair: (u8vector-ip . port-number)
(def (extract-source-ip client-addr)
  (cond
   ((not client-addr) "unknown")
   ((pair? client-addr)
    (let (ip (car client-addr))
      (if (u8vector? ip)
        (format "~a.~a.~a.~a"
                (u8vector-ref ip 0) (u8vector-ref ip 1)
                (u8vector-ref ip 2) (u8vector-ref ip 3))
        (object->string ip))))
   ((string? client-addr)
    (car (string-split client-addr #\:)))
   (else (object->string client-addr))))

;; Forward headers, filtering out hop-by-hop
(def (forward-headers headers)
  (filter (lambda (h)
            (not (member (car h)
                         '("Host" "Transfer-Encoding" "Connection"))))
          headers))

;; Core proxy handler
(def (proxy-handler req res)
  (let* ((id (uuid->string (random-uuid)))
         (method (http-request-method req))
         (path (http-request-path req))
         (params (http-request-params req))
         (headers (http-request-headers req))
         (body (http-request-body req))
         (client-addr (http-request-client req))
         (user (get-user-from-basic headers))
         (source (extract-source-ip client-addr))
         (agent (let (ua (assoc "User-Agent" headers))
                  (if ua (cdr ua) "")))
         (method-str (symbol->string method)))
    ;; Log request
    (log-request id user path params method-str source agent body)
    ;; Forward to backend
    (let* ((start-time (exact->inexact (time->seconds (##current-time))))
           (scheme (if (current-use-tls) "https" "http"))
           (query-part (if params (string-append "?" params) ""))
           (url (format "~a://~a:~a~a~a"
                        scheme
                        (current-remote-host)
                        (current-remote-port)
                        path
                        query-part))
           (fwd-headers (forward-headers headers))
           (ssl-ctx (and (current-use-tls) (insecure-client-ssl-context)))
           (resp (if ssl-ctx
                   (http-any method url
                             headers: fwd-headers
                             data: body
                             ssl-context: ssl-ctx)
                   (http-any method url
                             headers: fwd-headers
                             data: body))))
      (let* ((status (request-status resp))
             (resp-body (request-content resp))
             (resp-headers (request-headers resp))
             (content-length
              (let (cl (assoc "Content-Length" resp-headers))
                (if cl (cdr cl) (number->string (if (u8vector? resp-body) (u8vector-length resp-body) 0)))))
             (end-time (exact->inexact (time->seconds (##current-time))))
             (duration (- end-time start-time)))
        (request-close resp)
        ;; Log response
        (log-response id user path params method-str source agent status duration content-length)
        ;; Write response back to client
        (http-response-write res status
                             (filter (lambda (h)
                                       (not (member (car h)
                                                    '("Transfer-Encoding" "Connection"))))
                                     resp-headers)
                             resp-body)))))

;; Start the es-proxy server
(def (start-es-proxy local-host local-port)
  (let* ((mux (make-default-http-mux proxy-handler))
         (addr (format "~a:~a" local-host local-port))
         (srv (if (and (current-use-tls) (current-cert-path) (current-key-path))
                (start-http-server!
                 mux: mux
                 [ssl: addr (current-cert-path) (current-key-path)])
                (start-http-server! mux: mux addr))))
    (displayln (format "es-proxy ~a listening on ~a://~a"
                       VERSION
                       (if (current-use-tls) "https" "http")
                       addr))
    srv))

;; CLI entry point
(def (main . args)
  (call-with-getopt
   (lambda (opt)
     (let-hash opt
       (parameterize ((current-remote-host .remote-host)
                      (current-remote-port (string->number .remote-port))
                      (current-use-tls (not .?no-tls))
                      (current-cert-path (or .?cert (current-cert-path)))
                      (current-key-path (or .?key (current-key-path))))
         (let (srv (start-es-proxy .local-host (string->number .local-port)))
           (thread-join! srv)))))
   args
   program: "es-proxy"
   help: "Transparent ES proxy with request/response logging"
   (argument 'remote-host help: "Remote ES host")
   (argument 'remote-port help: "Remote ES port")
   (argument 'local-host help: "Local listen host")
   (argument 'local-port help: "Local listen port")
   (flag 'no-tls "--no-tls" help: "Disable TLS (plain HTTP)")
   (option 'cert "--cert" help: "TLS certificate path" default: #f)
   (option 'key "--key" help: "TLS key path" default: #f)))
