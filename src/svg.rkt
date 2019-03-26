#lang racket

(provide search-cord)
(require xml)
(require web-server/servlet
         web-server/servlet-env)
(require web-server/http/request-structs)

(define graph  '((1 (0 1) (3)) (2 (10 45) (3)) (3 (10 37) (1 2 4)) (4 (16 20) (6 5 3)) (5 (25 40) (4)) (6 (15 70) (4))))

(define (search-cord id)
  (cadr (assoc id graph)))

(search-cord 1)
(search-cord 2)
(search-cord 3)
(search-cord 4)
(search-cord 5)

(define (arc-with-cord node)
  (append (list (cadr node))
          (list (map search-cord (caddr node)))))

(arc-with-cord '(3 (10 37) (1 2 4)))
(arc-with-cord '(2 (10 45) (3)))
(arc-with-cord '(3 (10 37) (1 2 4)))
(arc-with-cord '(4 (16 20) (6 5 3)))
(arc-with-cord '(5 (25 40) (4)))

(define (arc-with-cord->graph graph)
  (map arc-with-cord graph))

(arc-with-cord->graph '((1 (0 1) (3)) (2 (10 45) (3)) (3 (10 37) (1 2 4)) (4 (16 20) (6 5 3)) (5 (25 40) (4))))


(define (path-string node neighbours)
      (string-append ))
      
      
;; An example of a page returning HTML with xexprs and macros
(define (display-page req)
  (response/xexpr
   `(html (head (title "OPEN MAPPING SERVICE DISPLAY PAGE"))
             (body 
              (svg ((width "500")
                    (height "300"))
                   (path ((d "M 3 8 L 45 79")
                          (stroke "red"))))))))
 


;; Routing function
;;     /display          --->   display-page
;;     everything else   --->   main-page
;(define-values (server-dispatch server-url)
 ;   (dispatch-rules
  ;  [("display") display-page]
   ;  [else main-page]))

(serve/servlet display-page
               #:servlet-regexp #rx""
               #:port 9000
               #:launch-browser? #f)
