#| Exercise 2.65

Use the results of Exercise 2.63 and Exercise 2.64 to give O(n)
implementations of `union-set' and `intersection-set' for sets
implemented as (balanced) binary trees. |#

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
          (let ((left-result (partial-tree elts left-size)))
            (let ((left-tree (car left-result))
                  (non-left-elts (cdr left-result))
                  (right-size (- n (+ left-size 1))))
              (let ((this-entry (car non-left-elts))
                    (right-result (partial-tree (cdr non-left-elts)
                                                right-size)))
                (let ((right-tree (car right-result))
                      (remaining-elts (cdr right-result)))
                  (cons (make-tree this-entry left-tree right-tree)
                        remaining-elts))))))))
  (car (partial-tree elements (length elements))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (entry set1)) (x2 (entry set2)))
           (cond ((= x1 x2)
                  (make-tree x1
                             (union-set (left-branch set1)
                                        (left-branch set2))
                             (union-set (right-branch set1)
                                        (right-branch set2))))
                 ((< x1 x2)
                  (make-tree x2
                             (union-set set1 (left-branch set2))
                             (right-branch set2)))
                 ((< x2 x1)
                  (make-tree x1
                             (union-set set2 (left-branch set1))
                             (right-branch set1))))))))

;; The above implementation doesn't quite work. The 3 element is duplicated.
(union-set (list->tree '(1 2 3)) (list->tree '(3 4 5)))
;;Value: (4 (3 (2 (1 () ()) (3 () ())) ()) (5 () ()))
#|
           4
          / \
         3   5
        /
       2
      / \
     1   3
|#

;; Can just be lazy(?) and reuse the O(n) procedures for ordered lists.

(define (union-set tree1 tree2)
  (define (union-ordered-list-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else
           (let ((x1 (car set1)) (x2 (car set2)))
             (cond ((= x1 x2)
                    (cons x1 (union-ordered-list-set (cdr set1) (cdr set2))))
                   ((< x1 x2)
                    (cons x1 (union-ordered-list-set (cdr set1) set2)))
                   ((< x2 x1)
                    (cons x2 (union-ordered-list-set set1 (cdr set2)))))))))
  (list->tree
   (union-ordered-list-set (tree->list tree1)
                           (tree->list tree2))))

(union-set (list->tree '(1 2 3)) (list->tree '(3 4 5)))
;;Value: (3 (1 () (2 () ())) (4 () (5 () ())))
#|
           3
         /   \
        1     4
         \     \
          2     5
|#


(define (intersection-set tree1 tree2)
  (define (element-of-ordered-list-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-ordered-list-set? x (cdr set)))))
  (define (intersection-ordered-list-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-ordered-list-set? (car set1) set2)
           (cons (car set1)
                 (intersection-ordered-list-set (cdr set1) set2)))
          (else (intersection-ordered-list-set (cdr set1) set2))))
  (list->tree
   (intersection-ordered-list-set (tree->list tree1)
                                  (tree->list tree2))))

(intersection-set (list->tree '(1 2 3 4 5)) (list->tree '(3 4 5 6 7)))
;;Value: (4 (3 () ()) (5 () ()))
#|
         4
       /   \
      3     5
|#
