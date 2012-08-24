#lang racket

(require xml)
(require net/url)
(require "./graphviz-rendering.rkt")
(require "./parse-http-request.rkt")


(define usage
  '(html
    (body
     (h1 "graphivz-server")
     (p "This is graphviz-server, an HTTP server.  When accepting a POST \
request with Graphviz Dot source code, it renders the source code into a \
PNG image cache on the server and returns an <IMG> tag of the image. \
Otherwise, it displays this information."))))

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (loop)))

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  ; Watcher thread:
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))

(define (handle in out)
  (let ((req (parse-http-request in)))
    (cond ((bytes=? (http-request-method req) #"POST")
           (handle-post-request req in out))
          (else
           (return-usage)))))

(define (respond/ok out)
  (display "HTTP/1.1 200 Okay\r\n" out)
  (display "Server: graphviz\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n" out))

(define (respond/bad-request out)
  (display "HTTP/1.1 400 Bad Request\r\n" out)
  (display "Server: graphviz\r\nContent-Type: text/plain\r\n\r\n" out))

(define (return-usage in out)
  (respond/ok out)
  (display (xexpr->string usage) out))

(define (handle-post-request req in out)
  (let* ((amt (second (assoc #"Content-Length"
                            (http-request-headers req))))
         (png-file
          (graphviz-render in
                           (string->number (bytes->string/utf-8 amt))
                           (cache-dir))))
    (cond ((path? png-file)
           (respond/ok out)
           (fprintf out "<img src=~a></img>\n"
                    (build-path (url-prefix) png-file)))
          (else
           (respond/bad-request out)))))
        

(define url-prefix (make-parameter ""))
(define cache-dir (make-parameter (current-directory)))
(define listen-port (make-parameter 9981))

(command-line
   #:once-each
   [("-u" "--url-prefix") prefix
    "URL prefix to the generated png files." (url-prefix prefix)]
   [("-d" "--cache-dir") dir
    "The directory on local filesystem for png files." (cache-dir dir)]
   [("-p" "--port") port
    "The port on which the server listens." (listen-port (string->number port))])

(serve (listen-port))
