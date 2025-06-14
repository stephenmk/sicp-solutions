#| Exercise 2.66

Implement the `lookup' procedure for the case where the set of records
is structured as a binary tree, ordered by the numerical values of the
keys. |#

(define (key tree) (car tree))
(define (entry tree) (cadr tree))
(define (left-branch tree) (caddr tree))
(define (right-branch tree) (cadddr tree))
(define (make-tree key entry left right)
  (list key entry left right))

(define (lookup given-key record-tree)
  (cond ((null? record-tree) false)
        ((= given-key (key record-tree))
         (entry record-tree))
        ((< given-key (key record-tree))
         (lookup given-key (left-branch record-tree)))
        ((> given-key (key record-tree))
         (lookup given-key (right-branch record-tree)))))

(define test-tree
  (make-tree 2
             'b
             (make-tree 1 'a '() '())
             (make-tree 3 'c '() '())))

(lookup 1 test-tree) ;Value: a
(lookup 2 test-tree) ;Value: b
(lookup 3 test-tree) ;Value: c
