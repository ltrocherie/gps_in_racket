#lang racket
(require "../src/conv_xml.rkt")
(require "../src/question2.rkt")
(require xml)

(define l '(
   (nd ((ref "2097959544")))
   "\n    "
   (nd ((ref "515330686")))
   "\n    "
   (tag ((k "highway") (v "tertiary")))
   "\n  "))

(define w1 '(way
             ((id "199797372"))
             "\n    "
             (nd ((ref "2097959544")))
             "\n    "
             (nd ((ref "515330686")))
             "\n    "
             (tag ((k "highway") (v "tertiary")))
             "\n  "))

(define w2 '(way
             ((changeset "65161805")
              (id "652338909")
              (timestamp "2018-12-04T14:08:52Z")
              (uid "2351029")
              (user "crln1")
              (version "1")
              (visible "true"))
             "\n  "
             (nd ((ref "6115852990"))) "\n  "
             (nd ((ref "6115852989"))) "\n  "
             (nd ((ref "6115852988"))) "\n  "
             (nd ((ref "6115852987"))) "\n  "
             (nd ((ref "6115852990"))) "\n  "
             (tag ((k "highway")
                   (v "yes")))
             "\n "))

(define w3 '(way
             ((changeset "54047686")
              (id "237095814")
              (timestamp "2017-11-24T11:37:37Z")
              (uid "138059")
              (user "Truchin")
              (version "2")
              (visible "true"))
             "\n  "
             (nd ((ref "5245587787")))
             "\n  "
             (nd ((ref "5245587788")))
             "\n  "
             (nd ((ref "2450447462")))
             "\n  "
             (nd ((ref "2450447460")))
             "\n  "
             (nd ((ref "2450447457")))
             "\n  "
             (nd ((ref "2450447456")))
             "\n  "
             (nd ((ref "2450447455")))
             "\n  "
             (nd ((ref "2450447458")))
             "\n  "
             (nd ((ref "2450447468")))
             "\n  "
             (nd ((ref "2450447473")))
             "\n  "
             (nd ((ref "2450447487")))
             "\n  "
             (nd ((ref "2450447490")))
             "\n  "
             (nd ((ref "2450447453")))
             "\n  "
             (nd ((ref "2450447454")))
             "\n  "
             (nd ((ref "312069978")))
             "\n  "
             (nd ((ref "312069453")))
             "\n  "
             (nd ((ref "312069450")))
             "\n  "
             (nd ((ref "312069449")))
             "\n  "
             (nd ((ref "312069448")))
             "\n  "
             (nd ((ref "312069447")))
             "\n  "
             (nd ((ref "312069446")))
             "\n  "
             (nd ((ref "312069445")))
             "\n  "
             (nd ((ref "312069444")))
             "\n  "
             (nd ((ref "2450447633")))
             "\n  "
             (nd ((ref "2450447632")))
             "\n  "
             (nd ((ref "2450447606")))
             "\n  "
             (nd ((ref "2450447595")))
             "\n  "
             (nd ((ref "2450447518")))
             "\n  "
             (nd ((ref "5245587783")))
             "\n  "
             (nd ((ref "2450447521")))
             "\n  "
             (nd ((ref "2450447515")))
             "\n  "
             (nd ((ref "2450447511")))
             "\n  "
             (nd ((ref "5245587787")))
             "\n  "
             (tag ((k "landuse") (v "farmland")))
             "\n  "
             (tag ((k "highway")
                   (v "cadastre-dgi-fr source : Direction Générale des Impôts - Cadastre. Mise à jour : 2013")))
             "\n ")
)

(define n1 '(nd ((ref "2097959544"))))
(define n2 '(nd ((ref "515330686"))))

(define t (xml->xexpr (document-element
                       (read-xml (open-input-file (or "maps/projMapping.osm" "maps/projMapping.osm"))))))

(define fr (xml->xexpr (document-element
                       (read-xml (open-input-file (or "maps/forrest.osm" "maps/forrest.osm"))))))

(define a (xml->xexpr (document-element
                       (read-xml (open-input-file "maps/lm.osm")))))


(printf "#########################################################################################\n")
(highway? w3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test de l'ouverture d'un fichier ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "#########################################################################################\n")
(printf "Test de l'ouverture d'un fichier xml basique\n")
;(xml->xexpr (document-element (read-xml (open-input-file (or "maps/projMapping.osm" "work/maps/projMapping.osm")))))

(printf "Test de l'ouverture d'un fichier xml complexe\n")
;(xml->xexpr (document-element (read-xml (open-input-file (or "maps/forrest.osm" "work/maps/forrest.osm")))))
;a
             

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test de roam-node ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "#########################################################################################\n")
(printf "Test de roam-node sur projMapping.osm\n")
;(roam-node t)
(printf "Test de roam-node sur forrest.osm\n")
;(roam-node fr)
(printf "Test de roam-node sur lm.osm\n")
;(roam-node a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test de roam-way ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "#########################################################################################\n")
(printf "Test de roam-way sur projMapping.osm\n")
(roam-way t)
(printf "Test de roam-way sur forrest.osm\n")
(roam-way fr)
(printf "Test de roam-way sur lm.osm\n")
(roam-way a)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test de roam-bounds ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "#########################################################################################\n")
(printf "Test de roam-bounds sur projMapping.osm\n")
;(roam-bounds t)
(printf "Test de roam-bounds sur forrest.osm\n")
;(roam-bounds fr)
(printf "Test de roam-bounds sur lm.osm\n")
;(roam-bounds a)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test de append-succs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "#########################################################################################\n")
(printf "Test de append-succ sur un fichier osm simple\n")
;(append-succs (roam-node t) (roam-way t))
(printf "Test de append-succ sur forrest.osm\n")
;(append-succs (roam-node fr) (roam-way fr))
(printf "Test de append-succ sur lm.osm\n")
;(append-succs (roam-node a) (roam-way a))

;(printf "Test de append-succ sur abeille.osm\n")
;(append-succs (roam-node a) (roam-way a))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test de voisins ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "#########################################################################################\n")
(printf "Test de voisins\n")
;(voisins '(1124048441 356 286) (roam-way t))

(printf "Test de graph_without_nodes_deg0&2\n")
;(graph_without_nodes_deg0&2_nodes (append-succs (roam-node t) (roam-way t)))