#lang racket

(require "./parse-http-request.rkt")
(require rackunit)

(define test-http-request
  "GET /work/6c30feb6a7662f58dc52f48eaead6b6d.png HTTP/1.1\r\n\
Accept: */*\r\n\
Referer: http://graphviz.server/graphviz-demo.html\r\n\
Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3\r\n\
\r\n")

(let* ((in1 (open-input-string test-http-request))
       (req (parse-http-request in1)))
  (check-equal? (http-request-method req) #"GET")
  (check-equal? (http-request-uri req)
                #"/work/6c30feb6a7662f58dc52f48eaead6b6d.png")
  (let ((headers (http-request-headers req)))
    (check-equal? (length headers) 3)
    (check-equal? (cdr (assoc #"Accept" headers)) #"*/*")
    (check-equal? (cdr (assoc #"Accept-Charset" headers))
                  #"ISO-8859-1,utf-8;q=0.7,*;q=0.3")
    (check-equal? (cdr (assoc #"Referer" headers))
                  #"http://graphviz.server/graphviz-demo.html")))
