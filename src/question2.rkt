#lang racket
(require xml)

;(xml->xexpr (document-element
 ;   (read-xml (open-input-file "../maps/projMapping.osm"))))

;;(define (simplify l)
  ;;(cadr l))

(define (del c1 c2) ;;concatene deux noeuds en supprimant l'element commun
   (cond
     [(= (car c1) (car c2)) (list (cadr c1) (cadr c2))]
     [(= (car c1) (cadr c2)) (list (cadr c1) (car c2))]
     [(= (cadr c1) (car c2)) (list (car c1) (cadr c2))]
     [else (list (car c1) (car c2))]))

;;(del '(13 12) '(12 14))

(define (all_nodes l) ;;concatene les couples dans une liste en gardant les occurences
  (if (null? l)
      '()
      (append (car l) (all_nodes (cdr l)))))

;;(all_nodes '((4 7) (4 6) (1 5)))

(define (insert_int x l) ;;insert un element dans une liste en gardant l'ordre
  (cond
    [(null? l) (cons x l)]
    [(>= (car l) x) (cons x l)]
    [(<= (car l) x) (cons (car l) (insert_int x (cdr l)))]))

(define (sort_numbers l) ;; trie une liste dans l'ordre croissant
  (if (null? l)
      l
      (insert_int (car l) (sort_numbers (cdr l)))))

;;(sort_numbers (all_nodes '((4 7) (4 6) (1 5))))

;;(define (useless_nodes sorted_nodes useless)
  ;;useless = listes des noeud apparaissant exactement 2 fois


;;(define (search n l )
  ;;(cond
    ;;[(=

(define (delete node l)
  (remove node l))

(define (manage_node n1 n2)  ;;ex (manage_node '(5 (1 4)) '(4 (9 5 6))) retourne '(4 (1 9 6)
  (append (list (car n2))
          (list (cons (cadadr n1) (remove (car n1) (cadr n2))))))

;(manage_node '(5 (1 4)) '(4 (9 5 6)))

(define (concat_neighbour node l) 
  (cons (manage_node node (assoc (caadr node) l))
        (remove (assoc (caadr node) l) l)))

(define (concat_neighbourS node l)
  (concat_neighbour (cons (car node) (list (reverse (cadr node))))
                    (concat_neighbour node l)))

(define (is_deg2? node)
  (if (= (length (cadr node)) 2)
      #t
      #f))
(concat_neighbourS '(2 (1 3)) '((1 (2)) (3 (2 4 7)) (4 (3 5 6)) (5 (4 6)) (6 (5 7)) (7 (6))))
(concat_neighbourS '(5 (4 6)) '((1 (2)) (3 (2 4 7)) (4 (3 5 6)) (5 (4 6)) (6 (5 7)) (7 (6))))
(concat_neighbourS '(6 (5 7)) '((1 (2)) (3 (2 4 7)) (4 (3 5 6)) (5 (4 6)) (6 (5 7)) (7 (6))))

;(is_deg2? '(5 (7 8)))

(define (nodes_deg2 l) ;;retourne la liste des noeuds de degré 2 extraits de l
  (if (null? l)
      '()
      (if (is_deg2? (car l))
          (cons (car l) (nodes_deg2 (cdr l)))
          (nodes_deg2 (cdr l)))))

;(nodes_deg2 '((1 (2)) (2 (1 3)) (3 (2 4 7)) (4 (3 5 6)) (5 (4 6)) (6 (5 7)) (7 (6))))

(define (remove_nodes_deg2 liste nodes_deg2_liste)
  (if (null? nodes_deg2_liste)
      liste
      (remove_nodes_deg2 (concat_neighbourS (car nodes_deg2_liste) liste) (cdr nodes_deg2_liste))))

(define (graph_without_deg2_nodes l)
  (remove_nodes_deg2 l (nodes_deg2 l)))

;(graph_without_deg2_nodes '((1 (2)) (2 (1 3)) (3 (2 4 7)) (4 (3 5 6)) (5 (4 6)) (6 (5 7)) (7 (6 0)))) A TERMINER
         
;;(delete '(6 (3 5)) '((4 (7 8)) (6 (3 5)) (1 (2 0))))
;(cons (car '(6 (3 5))) (list (reverse (cadr '(6 (3 5))))))
;(concat_neighbour '(6 (3 5)) '((3 (6 8)) (5 (1 6)) (1 (2 0))))
;(concat_neighbourS '(6 (3 5)) '((3 (6 8)) (5 (1 6)) (1 (2 0))))
;(caadr '(6 (3 5)))
;(assoc (caadr '(6 (3 5))) '((3 (6 8)) (5 (3 6)) (1 (2 0))))

;(nodes_deg2 '((3 (6 8 7)) (4 (6 8)) (7 (8 9 0)) (4 (0 9))))

;(assoc 5 '( (5 (6 7 5) (3 6))))