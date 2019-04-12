#lang racket

(require)
(provide (contract-out
 ;; Predicate identifying the vertices (returns true iff the argument
 ;; is a vertex)
 ;; @param  v : any/c
 ;; @return   : boolean?
 [vertex?  (-> any/c boolean?)]

 ;; Predicate identifying the graphs (returns true iff the argument is
 ;; a graph)
 ;; @param  g : any/c
 ;; @return   : boolean?
 [graph? (-> any/c boolean?)]

 ;; Constructor of a vertex
 ;; @param  id : exact-nonnegative-integer?
 ;; @return    : vertex?
 [vertex-new (-> exact-nonnegative-integer? vertex?)]
))

