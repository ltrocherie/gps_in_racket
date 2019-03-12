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

(roam-node (xml->xexpr (document-element
    (read-xml (open-input-file "../maps/forrest.osm")))))
