#lang racket

(require xml)
(require web-server/servlet
         web-server/servlet-env)

;requires internes
(require "interprete.rkt")

;; An example of a page returning TEXT with calls to fprintf
;; affiche maintenant la map vide en entier
(define (main-page req)
  (aff_blank_graph))
  

;; An example of a page returning HTML with xexprs and macros
(define (display-page req)
  (response/xexpr
   `(html (head (title "OPEN MAPPING SERVICE DISPLAY PAGE"))
             (body
              (h1 ,(url->string (request-uri req)))
              (pre ,(format "~a" (request-bindings req)))))))


;; fonction qui s'appelle quand on cherche l'URL route, qui interprète la
;; source et la destination demandée et qui affiche ce que renvoie (route start end)
(define (route-page req)
  (let ([listargs (request-bindings req)])
    (route (string->number (format "~a" (cdr (assoc 'start listargs))))
           (string->number (format "~a" (cdr (assoc 'end listargs)))))))

;;fonction appelée quand on cherche l'URL de la distance, qui interprète
;; start et end et qui affiche la distance minimale qui les sépare
(define (distance-page req)
  (let ([listargs (request-bindings req)])
    (distance (string->number (format "~a" (cdr (assoc 'start listargs))))
              (string->number (format "~a" (cdr (assoc 'end listargs)))))))

;;
;;
(define (cycle-page req)
  (let ([listargs (request-bindings req)])
    (cycle (map string->number (string-split (format "~a" (cdr (assoc 'nodes listargs))) ",")))))


;; Routing function
;;     /display          --->   display-page
;;     everything else   --->   main-page
(define-values (server-dispatch server-url)
    (dispatch-rules
     [("display") display-page]
     [("route") route-page]
     [("distance") distance-page]
     [("cycle") cycle-page]
     [else main-page]))

(serve/servlet server-dispatch
               #:servlet-regexp #rx""
               #:port 9000
               #:launch-browser? #f)
