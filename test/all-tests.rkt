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
                        (read-xml (open-input-file (or "maps/1_forrest.osm" "maps/1_forrest.osm"))))))

(define lm (xml->xexpr (document-element
                        (read-xml (open-input-file "maps/1_lm.osm")))))

(define a (xml->xexpr (document-element
                       (read-xml (open-input-file "maps/abeille.osm")))))


(define (main)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test de l'ouverture d'un fichier ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (printf "#######################\n")
  (printf "Test de l'ouverture d'un fichier xml basique : ")
  (xml->xexpr (document-element (read-xml (open-input-file (or "maps/projMapping.osm" "work/maps/projMapping.osm")))))
  (printf "OK\n")
  (printf "Test de l'ouverture d'un fichier xml complexe : ")
  (xml->xexpr (document-element (read-xml (open-input-file (or "maps/1_forrest.osm" "work/maps/1_forrest.osm")))))
  (printf "OK\n")
             

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test de roam-node ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (printf "#######################\n")
  (printf "Test de roam-node sur projMapping.osm : ")
  (roam-node t)
  (printf "OK\n")
  (printf "Test de roam-node sur forrest.osm : ")
  (roam-node fr)
  (printf "OK\n")
  (printf "Test de roam-node sur lm.osm : ")
  (roam-node a)
  (printf "OK\n")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test de roam-way ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (printf "#######################\n")
  (printf "Test de roam-way sur projMapping.osm : ")
  (roam-way t)
  (printf "OK\n")
  (printf "Test de roam-way sur forrest.osm : ")
  (roam-way fr)
  (printf "OK\n")
  (printf "Test de roam-way sur lm.osm : ")
  (roam-way lm)
  (printf "OK\n")
  (printf "Test de roam-way sur abeille.osm : ")
  (roam-way a)
  (printf "OK\n")


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test de roam-bounds ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (printf "#######################\n")
  (printf "Test de roam-bounds sur projMapping.osm : ")
  (roam-bounds t)
  (printf "OK\n")
  (printf "Test de roam-bounds sur forrest.osm : ")
  (roam-bounds fr)
  (printf "OK\n")
  (printf "Test de roam-bounds sur lm.osm : ")
  (roam-bounds a)
  (printf "OK\n")


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test de append-succs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (printf "#######################\n")
  (printf "Test de append-succ sur un fichier osm simple : ")
  (append-succs (roam-node t) (roam-way t))
  (printf "OK\n")
  (printf "Test de append-succ sur forrest.osm : ")
  (append-succs (roam-node fr) (roam-way fr))
  (printf "OK\n")
  (printf "Test de append-succ sur lm.osm : ")
  (append-succs (roam-node a) (roam-way a))
  (printf "OK\n")

  (printf "Test de append-succ sur abeille.osm : ")
  (append-succs (roam-node a) (roam-way a))
  (printf "OK\n")


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test de voisins ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (printf "#######################\n")
  (printf "Test de voisins : ")
  (voisins '(1124048441 356 286) (roam-way t))
  (printf "OK\n")

  (printf "Test de graph_without_nodes_deg0&2 : ")
  (graph_without_nodes_deg0&2_nodes (append-succs (roam-node t) (roam-way t)))
  (printf "OK\n")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test de highway ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (printf "#######################\n")
  (printf "Test de highway? : ")
  (highway? w3)
  (printf "OK\n")

(void))

(main)


