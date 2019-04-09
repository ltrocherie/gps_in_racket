#lang racket

(provide search-cord)
(require xml)
(require web-server/servlet
         web-server/servlet-env)
(require web-server/http/request-structs)

;;graphes de test
(define graph1  '((1 (100 150) (3)) (2 (10 45) (3)) (3 (280 89) (1 2 4)) (4 (16 20) (6 5 3)) (5 (25 40) (4)) (6 (15 70) (4))))

(define graph '((1127169432 (48.583211 4.9665652) (4899402261))
  (4899402261 (48.583807 4.9699008) (1127169432 1127169402 4899402262))
  (1127169383 (48.5830057 4.9665204) (4899402262))
  (4899402262 (48.5817725 4.9692631) (1127169383 1124048389 4899402261 1079892828))
  (1127169402 (48.5864335 5.0285572) (4899402261))
  (1124048389 (48.5643736 5.0088522) (4899402262))
  (1079892828 (48.5773455 4.9694463) (4899402262))))


(define (search-cord id graph)
  (cadr (assoc id graph)))
;(search-cord 1 graph)


(define (arc-with-cord node graph)
  (append (list (cadr node))
          (list (map (lambda (n) (search-cord n graph)) (caddr node)))))
;(arc-with-cord '(4 (16 20) (5 3)) '((1 (8 1) (3)) (2 (10 45) (3)) (3 (10 37) (1 2 4)) (4 (16 20) (5 3)) (5 (25 40) (4))))


(define (arc-with-cord->graph graph)
  (map (lambda (n) (arc-with-cord n graph)) graph))
;(arc-with-cord->graph '((1 (8 1) (3)) (2 (10 45) (3)) (3 (10 37) (1 2 4)) (4 (16 20) (5 3)) (5 (25 40) (4))))


(define (node-string n1 n2)
       (string-append "M "
                      (number->string (car n1))
                      " "
                      (number->string (cadr n1))
                      " L "
                      (number->string (car n2))
                      " "
                      (number->string (cadr n2))))


(define (paths-from-a-node node)
  (if (null? (cadr node))
      '()
      (cons (list 'path (list (list 'd (node-string (car node) (caadr node))) (list 'stroke "red")))
            (paths-from-a-node (append (list (car node)) (list (remove (caadr node) (cadr node))))))))
;(paths-from-a-node '((10 37) ((0 1) (10 45) (16 20))))

             
(define (paths-from-all-nodes graph)
  (if (null? graph)
      '()
      (append (paths-from-a-node (car graph)) (paths-from-all-nodes (cdr graph)))))
;(paths-from-all-nodes (arc-with-cord->graph '((1 (0 1) (3)) (2 (10 45) (3)) (3 (10 37) (1 2 4)) (4 (16 20) (6 5 3)) (5 (25 40) (4)))))


(define (create-svg width height graph)
  (append (list 'svg (list (list 'width width) (list 'height height))) (paths-from-all-nodes (arc-with-cord->graph graph))))
;(create-svg "300" "500" graph)


(define (create_html_response title width height graph)
  (list 'html
        (list 'head (list 'title title))
        (list 'body (create-svg width height graph))))
;(create_html_response "OPEN MAPPING SERVICE DISPLAY PAGE" "60" "6" graph)


(define (display-page req)
  (response/xexpr (create_html_response "OPEN MAPPING SERVICE DISPLAY PAGE" "60" "6" graph)))


(serve/servlet display-page
               #:servlet-regexp #rx""
               #:port 9000
               #:launch-browser? #f)
