#lang racket

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
