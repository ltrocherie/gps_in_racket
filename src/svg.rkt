#lang racket

(provide search-cord)
(provide create_html_response)
(provide create_html_responses)
(require xml)
(require "conv_xml.rkt")
(require web-server/servlet
         web-server/servlet-env)
(require web-server/http/request-structs)


;(define t (xml->xexpr (document-element
                       ;(read-xml (open-input-file "../maps/forrest.osm")))))

;(define data (append-succs (roam-node t) (roam-way t)))
;(roam-bounds t)
;graphes de test
;(define graph '((1127169432 (48.583211 4.9665652) (4899402261))
 ; (4899402261 (48.583807 4.9699008) (1127169432 1127169402 4899402262))
  ;;(1127169383 (48.5830057 4.9665204) (4899402262))
  ;(4899402262 (48.5817725 4.9692631) (1127169383 1124048389 4899402261 1079892828))
  ;(1127169402 (48.5864335 5.0285572) (4899402261))
  ;(1124048389 (48.5643736 5.0088522) (4899402262))
  ;(1079892828 (48.5773455 4.9694463) (4899402262))))



;;PRECOND: id un identifiant de noeaud, id>=0 et graph tq (is_graph? graph) --> #t
;;POSTCOND: valeur de retour, une liste de coordonnees
;;Action retourne les coordonnees du noeud d'identifiant id contenu dans graph
;;exemple: (search-cord 4899402262 graph) --> (48.5817725 4.9692631)
(define (search-cord id graph)
  (if (equal? (assoc id graph) #f)
      (printf "No corresponding node in the graph for this id")
      (cadr (assoc id graph))))
  

;;PRECOND: coord une liste de coordonnees de noeud respectivement avec lat et long, bounds une paire pointee comme ( (minlat . maxlat).(minlong . maxlong) )
;;POSTCOND: la meme liste de coordonnees
;;Action: converti la liste de coordonnees en entree de sorte à les agrandir en tenant compte des valeurs max et min des bornes lat et long
(define (coord_convertion coord bounds)
  (list (* (/ (- (car coord) (caar bounds)) (- (cdar bounds) (caar bounds))) 800) (* (/ (- (cadr coord) (caadr bounds)) (- (cdadr bounds) (caadr bounds))) 800)))  


;;PRECOND: node un noeud du graph tel specifié
;;POSTCOND: liste
;; action: retourne une liste contenant les coordonnes de l'id et une liste de coordonnes
;;         de tous ses voisins dans graphes convertie comme il faut avec la fonction precedente
;;(arc-with-cord '(1079892828 (48.5773455 4.9694463) (4899402262)) graph '(( 48 . 49) . (4.6 . 5.3)))
                      ;;--> '((461.8763999999999 422.22434285714365) ((465.41799999999967 422.0149714285718)))
(define (arc-with-cord node graph bounds)
  (append (list (coord_convertion (cadr node) bounds)
          (map (lambda (n) (coord_convertion n bounds)) (map (lambda (n) (search-cord n graph)) (caddr node))))))

;;applique la fonction precedente a tous les noeuds d'un graphe fournie en entree
;;exemple: (arc-with-cord->graph graph '(( 48 . 49) . (4.6 . 5.3)))
(define (arc-with-cord->graph graph bounds)
  (map (lambda (n) (arc-with-cord n graph bounds)) graph))


;;PRECOND: n1 et n2 des listes de coordonnees de type (lat long). Ces noeuds sont sensés etre voisins
;;POSTCOND: chaine de caractere
;;action: retourne la chaine de caractere "M n1.lat n1.long L n2.lat n2.long" pour specifier un chemin de n1 à n2
(define (node-string n1 n2)
       (string-append "M "
                      (number->string (car n1))
                      " "
                      (number->string (cadr n1))
                      " L "
                      (number->string (car n2))
                      " "
                      (number->string (cadr n2))))



;;PRECCOND: ATTENTION node-with-coord n'est pas un noeud du graphe comme specifié au debut mais plutot le retour de arc-with-coord c'est a dire
           ;;liste contenant coordonnees d'un point et liste de coordonnees de ses voisins
;;POSTCOND: liste de chaine de paths qui sera utilisé pour generer le svg
;;Action: realise les path pour le point avec chacun de ses voisins
;;exemple: (paths-from-a-node '((10 37) ((0 1) (10 45) (16 20))))
             ;;--> '((path ((d "M 10 37 L 0 1") (stroke "red"))) (path ((d "M 10 37 L 10 45") (stroke "red"))) (path ((d "M 10 37 L 16 20") (stroke "red"))))
(define (paths-from-a-node node-with-coord color)
  (if (null? (cadr node-with-coord))
      '()
      (cons (list 'path (list (list 'd (node-string (car node-with-coord) (caadr node-with-coord))) (list 'stroke color)))
            (paths-from-a-node (append (list (car node-with-coord)) (list (remove (caadr node-with-coord) (cadr node-with-coord)))) color))))



;;Realise la fonction precedente pour tout le graphe. Retourne donc la liste de liste de paths
(define (paths-from-all-nodes graph color)
  (if (null? graph)
      '()
      (append (paths-from-a-node (car graph) color) (paths-from-all-nodes (cdr graph) color))))


;;realise le cops du svg pour le graphe fournie en entree. width et heigh taille de la fenêtre souhaitée et bounds definie plus haut
(define (create-svg width height graph bounds color)
  (append (list 'svg (list (list 'width width) (list 'height height))) (paths-from-all-nodes (arc-with-cord->graph graph bounds) color)))


;;realise le format hml
;;exemple ;(create_html_response "OPEN MAPPING SERVICE DISPLAY PAGE" "8000" "1000" graph '(( 48 . 49) . (4.6 . 5.3)))
(define (create_html_response title width height graph bounds color)
  (list 'html
        (list 'head (list 'title title))
        (list 'body (create-svg width height graph bounds color))))


;;dans le meme format que la premiere mais prend en parametre 2 graphes
;;cette fonction permet d'afficher le map en une couleur pour un itinéraire au dessus en une autre couleur
(define (create_html_responses title width height graph1 graph2 bounds color1 color2)
  (list 'html
        (list 'head (list 'title title))
        (list 'body (create-svg width height graph1 bounds color1) (create-svg width height graph2 bounds color2))))
;(create_html_response "OPEN MAPPING SERVICE DISPLAY PAGE" "800" "800" data (roam-bounds t) "blue")

;test
;(define (display-page req)
 ; (response/xexpr (create_html_response "OPEN MAPPING SERVICE DISPLAY PAGE" "800" "800" data (roam-bounds t) "blue")))
;(serve/servlet display-page
     ;          #:servlet-regexp #rx""
    ;           #:port 9000
   ;            #:launch-browser? #t)