#lang racket

(require "./graphviz-rendering.rkt")

(require rackunit)
 
(check-equal?
 (path->string
  (graphviz-render
   (open-input-string
    "digraph graphname {\n\
     a -> b -> c;\n\
     b -> d;\n\
   }\n")
   "/tmp"))
  "ab0ee981691a12a1e947e330c18aae62.png")

(check-true
 (file-exists? "/tmp/ab0ee981691a12a1e947e330c18aae62.png"))
