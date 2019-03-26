#lang racket
(require xml)


;;Precond: n1 et n2 noeuds voisins et n1 de degre 2
;;Postcond: n2 avec voisin modifiés
;;Action: enleve n1 comme voisin de n2  et lui ajoute à n2 l'autre voisin de n1
;;exemple (manage_node '(5 (0.1 6) (1 4)) '(1 (2 8) (9 5 6))) --> '(1 (2 8) (9 4 6)))
(define (manage_node n1 n2) 
  (append (list (car n2) (cadr n2))
          (list (cons (cadr (caddr n1)) (remove (car n1) (caddr n2))))))


;;Precond: node un noeud de degre 2 et l une liste de noeuds
;;Postcond: l une liste de neouds
;;Action: applique manage_node au premier voisin de node qui serait dans l
;;exemple (concat_neighbour '(5 (0.1 6) (1 4)) '((5 (0.1 6) (1 4)) (1 (2 8) (9 5 6)) (4 (0 7) (5 3)))) --> '((5 (0.1 6) (1 4)) (1 (2 8) (9 4 6)) (4 (0 7) (5 3))))
(define (concat_neighbour node l)
  (if (equal? (assoc (caaddr node) l) #f)
      l
      (cons (manage_node node (assoc (caaddr node) l))
            (remove (assoc (caaddr node) l) l))))


;;Precond: node un noeud de degre 2 et l une liste de noeuds
;;Postcond: l une liste de neouds
;;Action: applique manage_node aux deux voisins de node qui seraient dans l
;;exemple: (concat_neighbourS '(5 (0.1 6) (1 4)) '((5 (0.1 6) (1 4)) (1 (2 8) (9 5 6)) (4 (0 7) (5 3)))) --> '((5 (0.1 6) (1 4)) (1 (2 8) (9 4 6)) (4 (0 7) (1 3))))
(define (concat_neighbourS node l)
  (concat_neighbour (append (list (car node) (cadr node)) (list (reverse (caddr node))))
                    (concat_neighbour node l)))


;;Precond: node un noeud
;;Postcond: un bouleen
;;Action: retourne #t si node est de degre2 et #f sinon
;;exemple: (is_deg2 '(5 (0.1 6) (1 4))) --> #t et (is_deg2 '(5 (0.1 6) (1 4))) --> #f
(define (is_deg2? node)
  (if (= (length (caddr node)) 2)
      #t
      #f))


;;Precons: une liste de noeuds
;;Postcond: une liste de noeuds
;;Action: retourne la liste de tous les noeuds de degree 2 contenues dans la liste en entree
;;exemple: (nodes_deg2 '((5 (0.1 6) (1 4)) (1 (2 8) (9 5 6)) (4 (0 7) (5 3)))) --> '((5 (0.1 6) (1 4)) (4 (0 7) (5 3))))
(define (nodes_deg2 l)
  (if (null? l)
      '()
      (if (is_deg2? (car l))
          (cons (car l) (nodes_deg2 (cdr l)))
          (nodes_deg2 (cdr l)))))


;;Precond: liste nodes_deg2_liste, deux listes de noeuds "nodes_deg2_liste" etant la liste des noeuds de degre 2 de "liste"
;;Postcond: une liste de noeuds
;;Action: enleve de "liste" tous les noeuds de degree 2 en ayant de soin de bien traiter d'abord les voisins de ceux-ci
;;exemple: (remove_nodes_deg2 '((1 (1 5) (2)) (3 (0.7 4) (2 4 7)) (4 (11 0.9) (3 5 9)) (7 (6 4) (6 3)) (5 (1 8.0) (4 6)) (6 (12 45) (5 7)) (2 (3 0) (1 3)))
;;                            '((1 (1 5) (2)) (7 (6 4) (6 3)) (5 (1 8.0) (4 6)) (6 (12 45) (5 7)) (2 (3 0) (1 3))) --> '((3 (0.7 4) (1 6 4)) (1 (1 5) (3)) (4 (11 0.9) (6 3 9)))
(define (remove_nodes_deg2 liste nodes_deg2_liste)
  (if (null? nodes_deg2_liste)
      liste
      (remove_nodes_deg2 (concat_neighbourS (car nodes_deg2_liste) (remove (assoc (caar nodes_deg2_liste) liste) liste))
                         (cdr nodes_deg2_liste))))
;;Precond: node un noeud
;;Postcond: un bouleen
;;Action: retourne #t si node est de degre0 et #f sinon
;;exemple: (is_deg2 '(5 (0.1 6) ())) --> #t et (is_deg2 '(5 (0.1 6) (1 4))) --> #f
(define (is_deg0? node)
  (if (= (length (caddr node)) 0)
      #t
      #f))


;;Precond: une liste representant la structure graphe
;;Poscond: une liste representant la structure graphe
;;Action: retire tous les noeuds de degre 0
;;exemple: (remove_nodes_deg0 '((1 (1 5) ()) (3 (0.7 4) (2 4 7)))) --> ((1 (1 5) ()))
(define (remove_nodes_deg0 liste)
  (if (null? liste)
      '()
      (if (is_deg0? (car liste))
          (remove_nodes_deg0 (cdr liste))
          (cons (car liste) (remove_nodes_deg0 (cdr liste))))))


;;Precond: l une liste de noeuds
;;Postcond: une liste de noeuds
;;Action: supprime d'une liste fournie tous des noeuds de degre 2 (sans qu'on fournisse la liste de ces noeuds en parametre)
;;        et ceux de degre 0.
;;exemple: (graph_without_nodes_deg0&2_nodes '((1 (1 5) (2)) (3 (0.7 4) (2 4 7)) (4 (11 0.9) (3 5 9)) (7 (6 4) (6 3)) (5 (1 8.0) (4 6)) (6 (12 45) (5 7)) (2 (3 0) (1 3)) (9 (4 5) ()))
                        ;;--> '((3 (0.7 4) (1 6 4)) (1 (1 5) (3)) (4 (11 0.9) (6 3 9)))
(define (graph_without_nodes_deg0&2_nodes l)
  (remove_nodes_deg0 (remove_nodes_deg2 l (nodes_deg2 l))))


