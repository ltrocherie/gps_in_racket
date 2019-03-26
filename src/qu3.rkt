#lang racket

;renvoie #t si n est dans la liste l, #f sinon
(define (is_in_list n l) 
  (if (null? l)
      #f
      (or (= n (car l)) (is_in_list n (cdr l)))))

;(is_in_list 3 '(1 3 5 8))
;(is_in_list 4 '(1 3 5 8))


;renvoie #t si un des élément de l apparait au moins deux fois dans l, #f sinon
(define (occ l)
  (if (null? l)
      #f
      (or (is_in_list (car l) (cdr l)) (occ (cdr l)))))

;(occ '(15 3 5 3))
;(occ '(15 8 3 5))
;(occ '())


;cherche tous les chemins du noeud beg vers le noeud end dans l
;remplie etrenvoie previous (qui doit initialement être '(beg))
;des différents chemins existant, renvoie'() sinon
(define (all_ways beg succ end l previous)
  (cond [(occ previous) '()]
        [(= beg end) (list previous)]
        [(not (null? succ)) (append (all_ways (car succ) (caddr (assoc (car succ) l)) end l (append previous (list (car succ))))
                                    (all_ways beg (cdr succ) end l previous))]
        [else '()]))

;(all_ways 1 '(2 4) 5 '((1 (2 4)) (2 (1 3 4)) (3 (2 5)) (4 (1 2 5)) (5 (3 4))) '(1))
;(all_ways 1 '(2 4) 6 '((1 (2 4)) (2 (1 3 4)) (3 (2 5)) (4 (1 2 5)) (5 (3 4)) (6 (7 8))) '(1))

;le nombre de chemins calculés pouvant rapidement devenir très important en
;augmentant le nombre de noeuds dans l, une version contenant le nombre de
;noeuds maximum acceptables pour un itinéraire peut être envisagée,
;évitant ainsi la recherche dans des directions inutiles 


;renvoie un itinéraire (succession de noeuds) reliant beg et end
;s'il en existe, '() sinon
(define (one_way beg end l)
  (if (null? (all_ways beg (caddr (assoc beg l)) end l (list beg)))
      '()
      (car (all_ways beg (caddr (assoc beg l)) end l (list beg)))))
      

;(one_way 1 5 '((1 (2 4)) (2 (1 3 4)) (3 (2 5)) (4 (1 2 5)) (5 (3 4))))
;(one_way 1 6 '((1 (2 4)) (2 (1 3 4)) (3 (2 5)) (4 (1 2 5)) (5 (3 4)) (6 (7 8))))
;(one_way 1 5 '((1 (0 0) (2 4)) (2 (1 0) (1 3 4)) (3 (1 1) (2 5)) (4 (2 1) (1 2 5)) (5 (1 2) (3 4))))

;renvoie l'itinéraire (succession de noeuds) passant par le moins de noeuds
;possible reliant beg et end s'il existe, '() sinon
;(define (least_nodes_way beg end l res)
;  (if (null? (all_ways beg (cadr (assoc beg l)) end l (list beg)))
;      '()
      
  


      

