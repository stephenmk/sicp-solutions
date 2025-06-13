#| Exercise 2.63

Each of the following two procedures converts a binary tree to a list. |#

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

#| a. Do the two procedures produce the same result for every tree? If
not, how do the results differ? What lists do the two procedures
produce for the trees in *Note Figure 2.16? |#

;;; Solution

;; The procedures should produce the same output for all trees.

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define tree1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))
(define tree2
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree 7
                        (make-tree 5 '() '())
                        (make-tree 9 '() (make-tree 11 '() '())))))
(define tree3
  (make-tree 5
             (make-tree 3 (make-tree 1 '() '()) '())
             (make-tree 9
                        (make-tree 7 '() '())
                        (make-tree 11 '() '()))))

(tree->list-1 tree1) ;Value: (1 3 5 7 9 11)
(tree->list-1 tree2) ;Value: (1 3 5 7 9 11)
(tree->list-1 tree3) ;Value: (1 3 5 7 9 11)
(tree->list-2 tree1) ;Value: (1 3 5 7 9 11)
(tree->list-2 tree2) ;Value: (1 3 5 7 9 11)
(tree->list-2 tree3) ;Value: (1 3 5 7 9 11)

#| b. Do the two procedures have the same order of growth in the number
of steps required to convert a balanced tree with n elements to a
list? If not, which one grows more slowly? |#

;;; Solution

;; The first procedure is less efficient because it is recursive in
;; space, while the latter is iterative. The first procedure makes
;; repeated calls to the `append' procedure and `cons', but the latter
;; only makes one call to `cons' for every call.
