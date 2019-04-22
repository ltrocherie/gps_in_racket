#lang racket

;(provide find_way)

;renvoie le carré de x
(define (square x)
  (* x x))

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
;remplie et renvoie previous (qui doit initialement être '(beg))
;des différents chemins existant, renvoie'() sinon
(define (all_ways beg succ end l previous)
  (cond [(occ previous) '()]
        [(= beg end) (list previous)]
        [(not (null? succ)) (append (all_ways (car succ) (caddr (assoc (car succ) l)) end l (append previous (list (car succ))))
                                    (all_ways beg (cdr succ) end l previous))]
        [else '()]))

;(all_ways 1 '(2 4) 5 '((1 (0 1) (2 4)) (2 (2 3)(1 3 4)) (3 (2 1) (2 5)) (4 (1 2) (1 2 5)) (5 (2 5) (3 4))) '(1))
;(all_ways 1 '(2 4) 6 '((1 (0 1) (2 4)) (2 (2 3)(1 3 4)) (3 (2 1) (2 5)) (4 (1 2) (1 2 5)) (5 (2 5) (3 4)) (6 (1 8) (7 8))) '(1))

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
      

;(one_way 1 5 '((1 (0 1) (2 4)) (2 (2 3)(1 3 4)) (3 (2 1) (2 5)) (4 (1 2) (1 2 5)) (5 (2 5) (3 4))))
;(one_way 1 6 '((1 (0 1) (2 4)) (2 (2 3)(1 3 4)) (3 (2 1) (2 5)) (4 (1 2) (1 2 5)) (5 (2 5) (3 4)) (6 (7 8))))

;renvoie la distance entre deux noeuds successifs de l
(define (dist_succ a b l)
  ;(display b)
  (sqrt (+ (square (- (caadr (assoc a l)) (caadr (assoc b l)))) (square (- (cadadr (assoc a l)) (cadadr (assoc b l)))))))

;(dist_succ 1 2 '((1 (0 0) (2 3 4)) (2 (5 3) (1 3 4))))

;renvoie le noeud le plus proche de a parmis ses successeurs dans l ainsi que la distance qui les sépare
;doit être appelé avec min = (car succ)
(define (nearest a succ l min)
  (cond [(null? succ) (list min (dist_succ min a l))]
        [(< (dist_succ a (car succ) l) (dist_succ a min l)) (nearest a (cdr succ) l (car succ))]
        [else (nearest a (cdr succ) l min)]))

;(nearest 1 '(2 3 4) '((1 (0 0) (2 3 4)) (2 (2 0) (1)) (3 (1 1) (1)) (4 (1 3) (1))) 2)

;renvoie #t si le nd n est dans la liste previous associée à l, #f sinon
(define (is_in_list2 n l) 
  (if (null? l)
      #f
      (if (= n (caar l))
          #t
          (is_in_list2 n (cdr l)))))

;(is_in_list2 3 '((4 2) (2 4) (2 3)))
;(is_in_list2 3 '((4 2) (5 1) (3 5)))
;(is_in_list2 6 '((4 2) (5 1) (3 5)))

;(define l '((1 (0 0) (2 3 4)) (2 (2 0) (1)) (3 (1 1) (1)) (4 (1 3) (1))))
;(display (caddr (assoc 1 l)))

;renvoie le triplé (nd, dist, prev_nd) du prochain noeud à utiliser dans find_way (Djikstra)
;s'initialise avec nodes='() et res=(car previous)
;(define (next previous nodes l res)
;  (if (null? previous)
;      res
;      (if (and (not (is_in_list (caar previous) nodes)) (< (+ (cadar previous) (dist_succ (caar previous) (nearest (caar previous)
;                                     (remq (caddar previous) (caddr (assoc (caar previous) l))) l (car (remq (caddar previous) (caddr (assoc (caar previous) l))))) l))
;                                                           (+ (cadr res) (dist_succ (car res) (nearest (car res) (remq (caddr res)
;                                                        (caddr (assoc (car res) l))) l (car (remq (caddr res) (caddr (assoc (car res) l))))) l))))
;          (next (cdr previous) (append nodes (cddar previous)) (car previous) l)
;          (next (cdr previous) (append nodes (cddar previous)) res l))))  

;(next '((3 2 1) (1 0 1)) '() '((1 (0 1) (3 4)) (2 (2 3)(3 4 5)) (3 (2 1) (2 1)) (4 (1 2) (1 2 5)) (5 (2 5) (2 4))) '(3 2 1))

;initialisation :
;res=(list (car previous) (+ (cadr (nearest (caar previous) (remq (caddar previous) (caddr (assoc (caar previous) l))) l (car (remq (caddar previous) (caddr (assoc (caar previous) l))))))))
;last_dist=(cadar previous)
(define (next previous l res last_dist)
  (if (null? previous)
      res
      (let ([new_dist (+ (cadar previous) (cadr (nearest (caar previous) (remq (caddar previous) (caddr (assoc (caar previous) l))) l (car (remq (caddar previous) (caddr (assoc (caar previous) l)))))))]
            [new_nd (car (nearest (caar previous) (remq (caddar previous) (caddr (assoc (caar previous) l))) l (car (remq (caddar previous) (caddr (assoc (caar previous) l))))))]) 
        (if (and (> new_dist last_dist) (< new_dist (cadr res)))
            (next (cdr previous) l (list new_nd new_dist (caar previous)) last_dist)
            (next (cdr previous) l res last_dist)))))

(let ([l '((1 (0 1) (3 4)) (2 (2 3)(3 4 5)) (3 (2 1) (2 1)) (4 (1 2) (1 2 5)) (5 (2 5) (2 4)))])
(next '((3 2 1) (4 1.41 1) (1 0 1)) l (list '(3 2 1) (+ 2 (cadr (nearest 3 (remq 1 (caddr (assoc 3 l))) l (car (remq 1 (caddr (assoc 3 l)))))))) 2))



;renvoie le chemin le plus court reliant beg et end dans l
;s'initialise avec nd=beg et previous='((beg 0 beg))
;(define (find_way_fct beg nd succ end l previous)
;  (cond [(and (is_in_list2 end previous) (= (car (next previous '() l (car previous))) end)) previous] 
;        [(null? succ) (find_way_fct beg (car (next previous '() l (car previous))) (caddr (assoc (car (next previous '() l (car previous))) l)) end l previous)]
;        [(is_in_list2 (car succ) previous) (find_way_fct beg nd (cdr succ) end l previous)]
;        [else (find_way_fct beg nd (cdr succ) end l (cons (list (car succ) (+ (dist_succ nd (car succ) l) (cadr (assoc nd previous))) nd) previous))]))

;(find_way_fct 1 1 '(3 4) 5 '((1 (0 1) (3 4)) (2 (2 3)(3 4 5)) (3 (2 1) (2 1)) (4 (1 2) (1 2 5)) (5 (2 5) (2 4))) '((1 0 1)))

;(define (find_way1 beg end l)
;  (find_way_fct beg beg (caddr (assoc beg l)) end l (list (list beg 0 beg))))
;(define (find_way2 beg end l)
  ;(list (cadr (assoc end (find_way_fct beg beg (caddr (assoc beg l)) end l (list (list beg 0 beg))))) (reverse (map car (find_way_fct beg beg (caddr (assoc beg l)) end l (list (list beg 0 beg)))))))

(define (extract-way djk src dst)
  (if (= src dst)
      (list src)
      (append (extract-way djk src (caddr (assoc dst djk))) (list dst))))

;doit renvoyer '() si les points sont déconnectés ou si les points n'existent pas
;(define (find_way beg end l)
;  (let ([djk (find_way1 beg end l)])
;    (if djk
;        (list (cadr (assoc end (find_way_fct beg beg (caddr (assoc beg l)) end l (list (list beg 0 beg))))) (extract-way djk beg end))
;        '())))

;(find_way1 1 5 '((1 (0 1) (3 4)) (2 (2 3)(3 4 5)) (3 (2 1) (2 1)) (4 (1 2) (1 2 5)) (5 (2 5) (2 4))))
;(find_way2 1 5 '((1 (0 1) (3 4)) (2 (2 3)(3 4 5)) (3 (2 1) (2 1)) (4 (1 2) (1 2 5)) (5 (2 5) (2 4))))
;(find_way 1 5 '((1 (0 1) (3 4)) (2 (2 3)(3 4 5)) (3 (2 1) (2 1)) (4 (1 2) (1 2 5)) (5 (2 5) (2 4))))
;PAS ENCORE LE MEILLEUR CHEMIN, REVOIR FIND_WAY_FCT  