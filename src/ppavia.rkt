#lang racket
(require xml)

;prend un triplet osm id-lat-long et renvoie le triplet de nombres correspondant
(define (create-node l) (list (extract-number (caar l)) (extract-number (cadar l)) (extract-number (caddar l))))

;prend un coupe symbole-string et traduit la string en nombre et renvoie ce nombre
(define (extract-number l) (string->number (cadr l)))

;parcourt une liste osm et renvoie la liste des nodes, formatés correctement
(define (roam-node t)
  (if (not (equal? '() t))
      (if (and (list? (car t)) (equal? (caar t) 'node))
          (cons (create-node (cdar t)) (roam-node (cdr t)))
      (roam-node (cdr t)))
  t)
)

(roam-node (xml->xexpr (document-element
    (read-xml (open-input-file "../maps/projMapping.osm")))))