#lang racket

(require)
(provide vertex-new)
(provide is_deg2?)
(provide is_deg0?)
(provide search-cord)
(provide conv-graph)
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
 ;[vertex-new (-> exact-nonnegative-integer? vertex?)]
))

(define (coord? c)
  (and (positive? (car c)) (positive? (cadr c))))

(define (neighbours? n)
  (or (null? n) (and (positive? (car n)) (neighbours? (cdr n)))))

;;vertex
(define (vertex? v)
  (and (positive? (car v)) (coord? (cadr v)) (neighbours? (caddr v))))

(define (vertex-new id coord neigh)
  (list id coord neigh))

(define (degree-vertex vertex)
  (length (caddr vertex)))

;;Precond: node un vertex
;;Postcond: un bouleen
;;Action: retourne #t si node est de degre2 et #f sinon
;;exemple: (is_deg2 '(5 (0.1 6) (1 4))) --> #t et (is_deg2 '(5 (0.1 6) (1 4))) --> #f
(define (is_deg2? node)
  (if (= (degree-vertex node) 2)
      #t
      #f))

;;Precond: node un noeud
;;Postcond: un bouleen
;;Action: retourne #t si node est de degre0 et #f sinon
;;exemple: (is_deg2 '(5 (0.1 6) ())) --> #t et (is_deg2 '(5 (0.1 6) (1 4))) --> #f
(define (is_deg0? node)
  (if (= (degree-vertex node) 0)
      #t
      #f))


(define (graph? g)
  (or (null? g) (and (vertex? (car g)) (graph? (cdr g)))))

(define (follower i l)
  (let ((res (member i l)))
    (if (null? (cdr res))
        '()
        (list (cadr res)))))

;;convertie une liste de id en du graphe g en un sous graphe de g
(define (conv-graph l g)
  (map (lambda (n) (vertex-new n (cadr (assoc n g)) (follower n l))) l))

;;PRECOND: id un identifiant de noeaud, id>=0 et graph tq (is_graph? graph) --> #t
;;POSTCOND: valeur de retour, une liste de coordonnees
;;Action retourne les coordonnees du noeud d'identifiant id contenu dans graph
;;exemple: (search-cord 4899402262 graph) --> (48.5817725 4.9692631)
(define (search-cord id graph)
  (if (equal? (assoc id graph) #f)
      (printf "No corresponding node in the graph for this id")
      (cadr (assoc id graph))))
