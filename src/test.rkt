#lang racket
(require "conv_xml.rkt")
(require xml)

(printf "Test de l'ouverture d'un fichier xml\n")
(xml->xexpr (document-element
             (read-xml (open-input-file "../maps/projMapping.osm"))))

(printf "Test de roam-node\n")
(roam-node (xml->xexpr (document-element
                        (read-xml (open-input-file "../maps/forrest.osm")))))

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

(printf "Test de append-succ\n")
(append-succs (roam-node t) (roam-way t))
(printf "Test de voisins\n")
(voisins '(1124048441 356 286) (roam-way t))