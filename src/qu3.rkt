#lang racket

;(provide find)
;(provide first-ways)
;(provide extract)
;(provide djk-inter)
;(provide dijkstra)
(provide find_way_djk)

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
;(oc1127169383c '(15 8 3 5))
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
  (sqrt (+ (square (- (caadr (assoc a l)) (caadr (assoc b l)))) (square (- (cadadr (assoc a l)) (cadadr (assoc b l)))))))

(define (haversine a b l)
  (let ([x_a (caadr (assoc a l))]
        [y_a (cadadr (assoc a l))]
        [x_b (caadr (assoc b l))]
        [y_b (cadadr (assoc b l))])
  (* 12742 (asin (sqrt (+ (square (sin (/ (- (/ (* pi x_a) 180) (/ (* pi x_b) 180)) 2))) (*(cos (/ (* pi x_a) 180)) (cos (/ (* pi x_b) 180)) (square (sin (/ (- (/ (* pi y_a) 180) (/ (* pi y_b) 180)) 2))))))))))
  
;(dist_succ 1 2 '((1 (0 0) (2 3 4)) (2 (5 3) (1 3 4))))
;(let ([lyon '(45.7597 4.8422)]
;      [paris '(48.8567 2.3508)])
;  (* 12742 (asin (sqrt (+ (square (sin (/ (- (/ (* pi (car lyon)) 180) (/ (* pi (car paris)) 180)) 2))) (*(cos (/ (* pi (car lyon)) 180)) (cos (/ (* pi (car paris)) 180)) (square (sin (/ (- (/ (* pi (cadr lyon)) 180) (/ (* pi (cadr paris)) 180)) 2))))))))) 
;normalement égal à 392.2172595594006 km
    
;renvoie le noeud le plus proche de a parmis ses successeurs dans l ainsi que la distance qui les sépare
;doit être appelé avec min = (car succ)
(define (nearest a succ l min)
  (cond [(null? succ) (list min (haversine min a l))]
        [(< (haversine a (car succ) l) (haversine a min l)) (nearest a (cdr succ) l (car succ))]
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

;(remq 1 '(2 1 3))
;(remq 1 '(2 4 3))

(define (nodes previous res)
  (if (null? previous)
      res
      (nodes (cdr previous) (cons (caar previous) res))))

;(let ((previous '((1 2 3) (3 4 5) (6 7 3))))
;  (nodes previous '())
;  (remq* (nodes previous '()) '(1 2 3 4 5 6)))

;initialisation :
;res=(list (car (nearest (caar previous) (remq (caddar previous) (caddr (assoc (caar previous) l))) l (car (remq (caddar previous) (caddr (assoc (caar previous) l))))))
;          (+ (cadar previous) (cadr (nearest (caar previous) (remq (caddar previous) (caddr (assoc (caar previous) l))) l (car (remq (caddar previous) (caddr (assoc (caar previous) l)))))))
;          (caar previous))
;last_dist=(cadar previous)
;nodes=(nodes previous '())
(define (next previous l res last_dist nodes)
  (if (null? previous)
      (if (is_in_list (car res) nodes)
          '()
          res)
      (let ([pot_succ (remq* nodes (caddr (assoc (caar previous) l)))])
        ;(display (caar previous))
        ;(display pot_succ)
             (if (not (null? pot_succ))
                 (let ([new_dist (+ (cadar previous) (cadr (nearest (caar previous) pot_succ l (car pot_succ))))]        
                       [new_nd (car (nearest (caar previous) pot_succ l (car pot_succ)))])
                   ;(display new_nd)
                   (if (and (> new_dist last_dist) (< new_dist (cadr res)))
                       ;(display new_dist)
                       ;(display "//")
                       ;(display last_dist)
                       (next (cdr previous) l (list new_nd new_dist (caar previous)) last_dist nodes)
                       (next (cdr previous) l res last_dist nodes)))
                  ;(display "plus de succ pos")
                  (next (cdr previous) l res last_dist nodes)))))
;tests
;(let ([l '((1 (0 1) (3 4)) (2 (2 3)(3 4 5)) (3 (2 1) (2 1)) (4 (1 2) (1 2 5)) (5 (2 5) (2 4)))])
;  (next '((4 1.42 1) (1 0 1)) l (list (car (nearest 4 (remq 1 (caddr (assoc 4 l))) l (car (remq 1 (caddr (assoc 4 l)))))) (+ 1.42 (cadr (nearest 4 (remq 1 (caddr (assoc 4 l))) l (car (remq 1 (caddr (assoc 4 l))))))) 4) 1.42))
;(let ([l '((1 (0 1) (3 4)) (2 (2 3)(3 4 5)) (3 (2 1) (2 1)) (4 (1 2) (1 2 5)) (5 (2 5) (2 4)))])
;  (next '((3 2 1) (4 1.42 1) (1 0 1)) l (list (car (nearest 3 (remq 1 (caddr (assoc 3 l))) l (car (remq 1 (caddr (assoc 3 l)))))) (+ 2 (cadr (nearest 3 (remq 1 (caddr (assoc 3 l))) l (car (remq 1 (caddr (assoc 3 l))))))) 3) 2))
;(let ([l '((1 (0 0) (2 3)) (2 (2 0) (1 3)) (3 (2 2) (2 1)) (4 (0 2) ()))]
;      [previous '((3 2.84 1) (2 2 1) (1 0 1))])
;  (next previous l (list (car (nearest 3 (remq 1 (caddr (assoc 3 l))) l (car (remq 1 (caddr (assoc 3 l)))))) (+ 2 (cadr (nearest 3 (remq 1 (caddr (assoc 3 l))) l (car (remq 1 (caddr (assoc 3 l))))))) 3) 2 (nodes previous '())))
;(display "OK")
;

;PB : Boucle infini car on autorise à refaire un chemin déjà fait

;renvoie le chemin le plus court reliant beg et end dans l
;s'initialise avec nd=beg et previous='((beg 0 beg))
(define (find_way_fct beg end l previous)
  (if (= end (caar previous))
      previous
      (let ([res (list (car (nearest (caar previous) (remq (caddar previous) (caddr (assoc (caar previous) l))) l (car (remq (caddar previous) (caddr (assoc (caar previous) l))))))
          (+ (cadar previous) (cadr (nearest (caar previous) (remq (caddar previous) (caddr (assoc (caar previous) l))) l
                                             (car (remq (caddar previous) (caddr (assoc (caar previous) l))))))) (caar previous))])
        (if (null? (next previous l res (cadar previous) (nodes previous '())))
            '()
            (find_way_fct beg end l (cons (next previous l res (cadar previous) (nodes previous '())) previous))))))
           
;(find_way_fct 1 6 '((1 (0 1) (3 4)) (2 (2 3)(3 4 5)) (3 (2 1) (2 1)) (4 (1 2) (1 2 5)) (5 (2 5) (2 4))) '((1 0 1)))
;(find_way_fct 1 5 '((1 (0 1) (3 4)) (2 (2 3)(3 4 5)) (3 (2 1) (2 1)) (4 (1 2) (1 2 5)) (5 (2 5) (2 4))) '((1 0 1)))
;(display 4)
;(define (find_way1 beg end l)
;  (find_way_fct beg beg (caddr (assoc beg l)) end l (list (list beg 0 beg))))
;(define (find_way2 beg end l)
;  (list (cadr (assoc end (find_way_fct beg beg (caddr (assoc beg l)) end l (list (list beg 0 beg))))) (reverse (map car (find_way_fct beg beg (caddr (assoc beg l)) end l (list (list beg 0 beg)))))))

;dist=0
(define (extract_way djk beg end dist)
  (if (= beg end)
      (list beg)
      (if (is_in_list2 end djk)
          (if (> (cadr (assoc end djk)) dist)
              (list (cadr (assoc end djk)) (append (extract_way djk beg (caddr (assoc end djk)) (cadr (assoc end djk))) (list end) ))
              (append (extract_way djk beg (caddr (assoc end djk)) dist) (list end)))
          '())))

(define (find_way_djk beg end l)
  (let ([way (extract_way (find_way_fct beg end l (list (list beg 0 beg))) beg end 0)])
    (if (null? way)
        '()
        way)))

;(define l '((1 (0 1) (3 4)) (2 (2 3)(3 4 5)) (3 (2 1) (2 1)) (4 (1 2) (1 2 5)) (5 (2 5) (2 4))))
;(find_way_djk 1 10 l)
;(find_way_djk 1 5 l)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TMP
(define (find beg end data prev)
  (if (= (caar prev) end)
      prev
      (find beg end data (cons (next prev
            data
            (list (car (nearest (caar prev)
                                (remq (caddar prev) (caddr (assoc (caar prev) data)))
                                data
                                (car (caddr (assoc (caar prev) data)))))
                  (+ (cadar prev)
                     (cadr (nearest (caar prev) (remq (caddar prev) (caddr (assoc (caar prev) data)))
                                    data
                                    (car (caddr (assoc (caar prev) data))))))
                  (caar prev))
            (cadar prev)
            (nodes prev '())) prev))))


(define (first-ways l)
  (list (caar l) (caddar l)))

(define (extract first-way l)
  (if (null? (cdr l))
      (append first-way (list (caar l)))
      (if ( = (cadr first-way) (caar l))
          (extract (append first-way (cddar l)) (cdr l))
          (extract first-way (cdr l)))))

;;l is the return of find
(define (djk-inter beg end l)
  (list (cadar l) (reverse (extract (first-ways l) (cdr l)))))

(define (dijkstra beg end data)
  (djk-inter beg end (find beg end data (list (list beg 0 beg)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END TMP


;doit renvoyer '() si les points sont déconnectés ou si les points n'existent pas
;(define (find_way beg end l)
;  (let ([djk (find_way1 beg end l)])
;    (if djk
;        (list (cadr (assoc end (find_way_fct beg beg (caddr (assoc beg l)) end l (list (list beg 0 beg))))) (extract-way djk beg end))
;        '())))

;(find_way1 1 5 '((1 (0 1) (3 4)) (2 (2 3)(3 4 5)) (3 (2 1) (2 1)) (4 (1 2) (1 2 5)) (5 (2 5) (2 4))))
;(find_way2 1 5 '((1 (0 1) (3 4)) (2 (2 3)(3 4 5)) (3 (2 1) (2 1)) (4 (1 2) (1 2 5)) (5 (2 5) (2 4))))
;(find_way 1 5 '((1 (0 1) (3 4)) (2 (2 3)(3 4 5)) (3 (2 1) (2 1)) (4 (1 2) (1 2 5)) (5 (2 5) (2 4))))