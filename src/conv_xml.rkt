#lang racket
(require xml)

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(xml->xexpr (document-element
  ;;  (read-xml (open-input-file "../maps/projMapping.osm"))))

;;(roam-node (xml->xexpr (document-element
  ;;  (read-xml (open-input-file "../maps/forrest.osm")))))

(define t (xml->xexpr (document-element
                       (read-xml (open-input-file "../maps/forrest.osm")))))
(define w '(way
   ((id "199797372"))
   "\n    "
   (nd ((ref "2097959544")))
   "\n    "
   (nd ((ref "515330686")))
   "\n    "
   (tag ((k "highway") (v "tertiary")))
   "\n  "))

(define l '(
   (nd ((ref "2097959544")))
   "\n    "
   (nd ((ref "515330686")))
   "\n    "
   (tag ((k "highway") (v "tertiary")))
   "\n  "))

(define n1 '(nd ((ref "2097959544"))))
(define n2 '(nd ((ref "515330686"))))

(roam-way t)


