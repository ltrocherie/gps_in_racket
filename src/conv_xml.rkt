#lang racket
(require xml)

(provide append-succs roam-node roam-way roam-bounds voisins append-succs)

;prend un morceau osm contenant id lat et long et renvoie le triplet de nombres correspondant
(define (create-node l) (list (extract-tag 'id (car l)) (extract-tag 'lat (car l)) (extract-tag 'lon (car l))))

;prend un tag et une paire osm et renvoie le nombre associé
(define (extract-tag tag l)
  (if (equal? tag (caar l))
      (convert-number (car l))
      (extract-tag tag (cdr l))
  )
)

;prend un coupe symbole-string et traduit la string en nombre et renvoie ce nombre
(define (convert-number l) (string->number (cadr l)))




;parcourt une liste osm et renvoie la liste des nodes, formatés correctement
(define (roam-node t)
  (if (not (equal? '() t))
      (if (and (list? (car t)) (equal? (caar t) 'node))
          (cons (create-node (cdar t)) (roam-node (cdr t)))
      (roam-node (cdr t)))
  t)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;teste si l'en-tête de liste correspond au tag passé en paramètre
(define (tag? tag l)
  (equal? (car l) tag))


;;teste si la ligne tag passée en paramètre correspond à une highway
(define (highway-tag? l)
  (equal? (car (cdaadr  l)) "highway"))

;;teste si la way privée de son symbole et de son id est une highway
(define (highway-list? w)
  (if (and (list? (car w)) (tag? 'tag (car w)))
      (highway-tag? (car w))
      (highway-list? (cdr w)))
  )

;;teste si la way est une highway
(define (highway? w)
  (highway-list? (cddr w)))

;;construit une paire de références à partir de deux lignes en nd
(define (pair l1 l2)
  (list (convert-number (caadr l1)) (convert-number (caadr l2)))
  )

;;construit la liste de paires(segments) à partir d'une way privée de son symbole et de son id
(define (wtp-rec l)
  (if (and (tag? 'nd (car l)) (tag? 'nd (caddr l)))
      (cons (pair (car l) (caddr l)) (wtp-rec (cddr l)))
      '()
  ))

;;construit la liste de paires(segments) à partir d'une way
(define (way-to-pairs w)
  (if (highway? w)
      (wtp-rec (cdddr w))
      '()
  ))

;;construit la liste de segments à partir d'un osm
(define (roam-way t)
  (if (not (equal? '() t))
      (if (and (list? (car t)) (equal? (caar t) 'way))
          (append (way-to-pairs (car t)) (roam-way (cdr t)))
          (roam-way (cdr t)))
  t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;construit une liste de noeuds avec successeurs à partir d'une liste de noeuds et d'une liste d'arcs
(define (append-succs noeuds paires)
  (if (equal? noeuds '())
      '()
      (cons (list (caar noeuds) (cdar noeuds) (voisins (car noeuds) paires)) (append-succs (cdr noeuds) paires))
      )
)

(define (voisins noeud paires)
  (if (equal? paires '())
      '()
      (if (equal? (car noeud) (caar paires))
          (cons (cadar paires) (voisins noeud (cdr paires)))
          (if (equal? (car noeud) (cadar paires))
              (cons (caar paires) (voisins noeud (cdr paires)))
              (voisins noeud (cdr paires))
              )
          )
      )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;prend un morceau osm contenant bounds maxlat, maxlon, minlat et minlong et renvoie la paire
;pointée correspondant au tag passé en paramètre dans l'ordre min . max
(define (create-bound tag l)
  (if (equal? tag 'lat)
      (cons (convert-number (car (cddadr l))) (convert-number (caadr l)))
      (cons (convert-number (cadr (cddadr l))) (convert-number (cadadr l)))
  ))

;parcourt une liste osm et renvoie la liste des nodes, formatés correctement
(define (roam-bounds t)
  (if (not (equal? '() t))
      (if (and (list? (car t)) (equal? (caar t) 'bounds))
          (list (create-bound 'lat (car t)) (create-bound 'lon (car t)))
      (roam-bounds (cdr t)))
  t)
)