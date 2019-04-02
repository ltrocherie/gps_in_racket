#lang racket
(require "../src/conv_xml.rkt")
(require "../src/question2.rkt")
(require xml)

(printf "Test de l'ouverture d'un fichier xml\n")
(xml->xexpr (document-element
             (read-xml (open-input-file (or "maps/projMapping.osm" "work/maps/projMapping.osm")))))

(printf "Test de roam-node\n")
(roam-node (xml->xexpr (document-element
                        (read-xml (open-input-file (or "maps/projMapping.osm" "maps/projMapping.osm"))))))
                                                       
(define t (xml->xexpr (document-element
                       (read-xml (open-input-file (or "maps/projMapping.osm" "maps/projMapping.osm"))))))
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

(printf "Test de graph_without_nodes_deg0&2\n")
(graph_without_nodes_deg0&2_nodes (append-succs (roam-node t) (roam-way t)))