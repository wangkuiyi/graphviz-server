#lang racket

(define test-http-request
  "GET /work/6c30feb6a7662f58dc52f48eaead6b6d.png HTTP/1.1\r\n\
Host: graphviz.server\r\n\
Connection: keep-alive\r\n\
User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_4) AppleWebKit/536.11 (KHTML, like Gecko) Chrome/20.0.1132.57 Safari/536.11\r\n\
Accept: */*\r\n\
Referer: http://graphviz.server/graphviz-demo.html\r\n\
Accept-Encoding: gzip,deflate,sdch\r\n\
Accept-Language: en-US,en;q=0.8\r\n\
Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3\r\n\
\r\n")

(struct http-request (method uri headers)
        #:transparent)

;; Parse the first line of an HTTP request.  Returns '(method URI) if
;; succeeded, or #f otherwise.
(define (parse-request-line in)
  (let ((matched (regexp-match #rx"([^ ]+) ([^ ]+) ([^ ]+)\r\n" in)))
    (if (false? matched)
        #f
        (list (second matched) (third matched)))))

;; Parse headers as an alist.
(define (parse-headers in header-list)
  (let ((header (regexp-match #rx"([^:]+): ([^\r]+)\r\n" in)))
    (if (false? header)
        header-list
        (parse-headers in
                       (cons (cons (second header) (third header))
                             header-list)))))

;; Parse the HTTP request as composed by a request line and a head list.
(define (parse-http-request in)
  (let* ((request-line (parse-request-line in)))
    (cond ((false? request-line)
           #f)
          (else
           (let ((header-list (parse-headers in '())))
             (cond ((empty? header-list)
                    #f)
                   (else
                    (http-request (first request-line)
                                  (second request-line)
                                  header-list))))))))

(provide (struct-out http-request)
         parse-http-request)
