#lang racket
;ce fichier importe le code de la question 3 (et 4 et 5 dans le futur)
;il affiche en HTML/HTTP le chemin le plus court entre deux points
;il est l'intermédiaire entre l'algo de recherche de chemin et les fonctions
;d'affichage graphique SVG

(require "graph.rkt")
(require "conv_xml.rkt")
(require "qu3.rkt")
(require "question2.rkt")
(require "svg.rkt")
(require xml)
(require web-server/servlet)

(provide route)
(provide distance)
(provide cycle)
(provide aff_blank_graph)

;en attendant d'avoir la fonction qui renvoie du response/xexpr
;avec le chemin demandé
(define (aff_graph argue)
  (response/xexpr (create_html_responses "OPEN MAPPING SERVICE REAL DISPLAY PAGE" "800" "800" data argue (roam-bounds t) "black" "red")
))

;affiche la map vide
(define (aff_blank_graph)
  (response/xexpr (create_html_response "OPEN MAPPING SERVICE REAL DISPLAY PAGE" "800" "800" data (roam-bounds t) "black"))
)


;cherche un chemin entre les points start et end
;si le chemin existe, elle renvoie un response/xexpr avec le code HTML SVG
;qui affiche le chemin
;sinon, elle affiche le message d'erreur demandé
(define (route start end)
  ;(let ([onwei (one_way start end data)]);avec one_way la fonction qui trouve le chemin entre start et end à travers data
  (let ([findwei (find_way start end data)]);avec find_way la fonction qui trouve le chemin entre start et end à travers data
    (if (null? findwei)
        (response/output
         #:mime-type TEXT/HTML-MIME-TYPE
         (lambda (out)
           (fprintf out "Disconnected Universe Error~n")
           ));renvoie l'erreur demandée
        ;(aff_graph (shapeshift (cdr findwei)))
        (aff_graph (conv-graph (cadr findwei) data)) ;maintenant le cdr est sous la forme graph
        )))

;cherche le chemin le plus court entre start et end
;renvoie sa distance en response/xexpr avec le code HTML SVG si il existe
;renvoie le message d'erreur sinon
(define (distance start end)
  (let ([findwei (find_way start end data)]);avec find_way la fonction qui trouve le chemin entre start et end à travers data
    (if (null? findwei)
        (response/output
         #:mime-type TEXT/HTML-MIME-TYPE
         (lambda (out)
           (fprintf out "Disconnected Universe Error~n")
           ));renvoie l'erreur demandée
        (response/output
         #:mime-type TEXT/HTML-MIME-TYPE
         (lambda (out)
           (fprintf out (string-append "La distance demandée est : " (number->string (car findwei)) "."))
           ));renvoie la distance demandée
        )))

;cherche le cycle le plus court reliant tous les points de la liste nodes
;affiche soit la représentation graphique de l'itinéraire circulaire
;soit le message d'erreur
(define (cycle nodes)
  #t);à finir

(define t (xml->xexpr (document-element
                       (read-xml (open-input-file (command-line #:args (filename) filename))))))
                       ;(read-xml (open-input-file "./maps/forrest-testloop.osm")))))
(define data (graph_without_nodes_deg0&2_nodes(append-succs (roam-node t) (roam-way t))))