#lang racket

(provide search-cord)
(require xml)
(require web-server/servlet
         web-server/servlet-env)
(require web-server/http/request-structs)

(define graph  '((1 (100 150) (3)) (2 (10 45) (3)) (3 (280 89) (1 2 4)) (4 (16 20) (6 5 3)) (5 (25 40) (4)) (6 (15 70) (4))))

(define (search-cord id graph)
  (cadr (assoc id graph)))

;(search-cord 1)
;(search-cord 2)
;(search-cord 3)
;(search-cord 4)
;(search-cord 5)

(define (arc-with-cord node)
  (append (list (cadr node))
          (list (map search-cord (caddr node)))))

;(search-cord 1124048547); 4899402262))))

;(;arc-with-cord '(1127169432 (48.583211 4.9665652) (4899402261)))

;(arc-with-cord '(4899402261 (48.583807 4.9699008) (1127169432 1124048547 4899402262)))
;(arc-with-cord '(1127169383 (48.5830057 4.9665204) (4899402262)))
;(arc-with-cord '(4899402262
 ;  (48.5817725 4.9692631)
;   (1127169383 1124048389 4899402261 1079892828)))
;(arc-with-cord '(1127169402 (48.5864335 5.0285572) (1124048485)))

(define (arc-with-cord->graph graph)
  (map arc-with-cord graph))

;(arc-with-cord->graph '((1 (8 1) (3)) (2 (10 45) (3)) (3 (10 37) (1 2 4)) (4 (16 20) (6 5 3)) (5 (25 40) (4))))


(define (node-string n1 n2)
       (string-append "M "
                      (number->string (car n1))
                      " "
                      (number->string (cadr n1))
                      " L "
                      (number->string (car n2))
                      " "
                      (number->string (cadr n2))))
;(define (my-path d n)
 ; (path ((d n))))
;(define (d n)
 ; d n)

;;(my-map d "f" 
;(node-string '(4 8) '(8 6))

(define (paths-from-a-node node)
  (if (null? (cadr node))
      '()
      (cons (list 'path (list (list 'd (node-string (car node) (caadr node))) (list 'stroke "red")))
            (paths-from-a-node (append (list (car node)) (list (remove (caadr node) (cadr node))))))))
             
(define (paths-from-all-nodes graph)
  (if (null? graph)
      '()
      (append (paths-from-a-node (car graph)) (paths-from-all-nodes (cdr graph)))))
      
;(path-string '(25 40) '(16 20))
;(caadr '((10 37) ((0 1) (10 45) (16 20))))
;(list (node-string (car '((10 37) ((0 1) (10 45) (16 20)))) (caadr '((10 37) ((0 1) (10 45) (16 20))))))
;(paths-from-a-node '((10 37) ((0 1) (10 45) (16 20))))
;(list (remove (caadr '((10 37) ((0 1) (10 45) (16 20)))) (cadr '((10 37) ((0 1) (10 45) (16 20))))))


;(paths-from-all-nodes (arc-with-cord->graph '((1 (0 1) (3)) (2 (10 45) (3)) (3 (10 37) (1 2 4)) (4 (16 20) (6 5 3)) (5 (25 40) (4)))))
;; An example of a page returning HTML with xexprs and macros

(define (create-svg width height graph)
  (append (list 'svg (list (list 'width width) (list 'height height))) (paths-from-all-nodes (arc-with-cord->graph graph))))

;(create-svg "300" "500" graph)

(define (create_html_response title width height graph)
  (list 'html
        (list 'head (list 'title title))
        (list 'body (create-svg width height graph))))

(create_html_response "OPEN MAPPING SERVICE DISPLAY PAGE" "300" "500" graph2)


(define (display-page req)
  (response/xexpr (create_html_response "OPEN MAPPING SERVICE DISPLAY PAGE" "300" "500" graph2)))
   




;; Routing function
;;     /display          --->   display-page
;;     everything else   --->   main-page
;(define-values (server-dispatch server-url)
 ;   (dispatch-rules
  ;  [("display") display-page]
   ;  [else main-page]))

;;(serve/servlet display-page
;               #:servlet-regexp #rx""
 ;              #:port 9000
  ;             #:launch-browser? #f)
