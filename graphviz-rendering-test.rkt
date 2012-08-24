#lang racket

(require "./graphviz-rendering.rkt")
(require rackunit)

(define png-file "/tmp/ab0ee981691a12a1e947e330c18aae62.png")

(define dot-source
  "digraph graphname {\n\
     a -> b -> c;\n\
     b -> d;\n\
   }\n")

(when (file-exists? png-file)
  (delete-file png-file))

(check-false
 (file-exists? "/tmp/ab0ee981691a12a1e947e330c18aae62.png"))

(check-equal?
 (path->string
  (graphviz-render
   (open-input-string dot-source)
   (string-length dot-source)
   "/tmp"))
  "ab0ee981691a12a1e947e330c18aae62.png")

(check-true
 (file-exists? "/tmp/ab0ee981691a12a1e947e330c18aae62.png"))
