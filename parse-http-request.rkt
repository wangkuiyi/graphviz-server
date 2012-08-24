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

(define (parse-header in)
  (if (char=? (peek-char in) #\return)
      #f
      (let ((header (regexp-match #rx"((([^:]+): ([^\r]+))|^)\r\n" in)))
        (list (fourth header) (fifth header)))))

(define (parse-headers in hds)
  (let ((hd (parse-header in)))
    (cond ((false? hd) hds)
          (else (parse-headers in (cons hd hds))))))

;; Parse the HTTP request as composed by a request line and a head list.
(define (parse-http-request in)
  (let* ((req (parse-request-line in))
         (hds (parse-headers in '())))
    (http-request (first req) (second req) hds)))

(provide (struct-out http-request)
         parse-http-request)
