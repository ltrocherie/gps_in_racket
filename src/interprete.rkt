#lang racket
;ce fichier importe le code de la question 3 (et 4 et 5 dans le futur)
;il affiche en HTML/HTTP le chemin le plus court entre deux points
;il est l'intermédiaire entre l'algo de recherche de chemin et les fonctions
;d'affichage graphique SVG

(require "conv_xml.rkt")
(require "qu3.rkt")
(require "svg.rkt")
(require xml)
(require web-server/servlet)

(provide route)

;en attendant d'avoie la fonction qui renvoie du response/xexpr
;avec le chemin demandé
(define (aff_graph a b) #f)

;cherche un chemin entre les points start et end
;si le chemin existe, elle renvoie un response/xexpr avec le code HTML SVG
;qui affiche le chemin
;sinon, elle affiche le message d'erreur demandé
(define (route start end)
  (let ([onwei (one_way start end data)])
  (if (null? onwei)
      (response/output
        #:mime-type TEXT/HTML-MIME-TYPE
        (lambda (out)
          (fprintf out "Disconnected Universe Error~n")
          ));renvoie l'erreur demandée
      (aff_graph (shapeshift onwei))
      )))

;transforme une liste d'ID en liste de (coordonnées ((coordonnées du suivant)))
(define (shapeshift l)
  (if (null? l)
      l
      (if (null? (cdr l))
          (cdr l)
          (cons (list (search-cord (car l)) (list (search-cord (cadr l)))) (shapeshift (cdr l)))
          )
      )
  )

(define t (xml->xexpr (document-element
                       (read-xml (open-input-file "../maps/forrest-testloop.osm")))))
(define data (append-succs (roam-node t) (roam-way t)))

;(define (distance start end))