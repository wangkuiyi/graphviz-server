#lang racket

(require "./parse-http-request.rkt")
(require rackunit)

(define test-http-request
  "POST /graphviz/ HTTP/1.0\r\n\
X-Scheme: http\r\n\
Host: graphviz.server:9981\r\n\
Connection: close\r\n\
Content-Length: 71\r\n\
Cache-Control: max-age=0\r\n\
Origin: http://graphviz.server\r\n\
Content-Type: text/plain\r\n\
Accept: */*\r\n\
Referer: http://graphviz.server/graphviz-demo.html\r\n\
Accept-Encoding: gzip,deflate,sdch\r\n\
Accept-Language: en-US,en;q=0.8\r\n\
Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3\r\n\
\r\n\
      digraph graphname {\n\
      a -> b -> c;\n\
      b -> d;\n\
      }\n\
")

(let* ((in1 (open-input-string test-http-request))
       (req (parse-http-request in1)))
  (copy-port in1 (current-output-port))
  (check-equal?
   req
   (http-request #"POST"
                 #"/graphviz/"
                 '((#"Accept-Charset" #"ISO-8859-1,utf-8;q=0.7,*;q=0.3")
                   (#"Accept-Language" #"en-US,en;q=0.8")
                   (#"Accept-Encoding" #"gzip,deflate,sdch")
                   (#"Referer" #"http://graphviz.server/graphviz-demo.html")
                   (#"Accept" #"*/*")
                   (#"Content-Type" #"text/plain")
                   (#"Origin" #"http://graphviz.server")
                   (#"Cache-Control" #"max-age=0")
                   (#"Content-Length" #"71")
                   (#"Connection" #"close")
                   (#"Host" #"graphviz.server:9981")
                   (#"X-Scheme" #"http")))))
