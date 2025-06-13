#| Exercise 2.62

Give an O(n) implementation of `union-set' for sets represented as
ordered lists. |#

;;; Solution
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 ((< x2 x1)
                  (cons x2 (union-set set1 (cdr set2)))))))))

;;; Tests
(union-set '(1 2 3) '())        ;Value: (1 2 3)
(union-set '() '(1 2 3))        ;Value: (1 2 3)
(union-set '(1 2 3) '(1 2 3))   ;Value: (1 2 3)
(union-set '(1 2 3) '(4 5 6))   ;Value: (1 2 3 4 5 6)
(union-set '(4 5 6) '(1 2 3))   ;Value: (1 2 3 4 5 6)
(union-set '(1 3 5) '(2 4 6 8)) ;Value: (1 2 3 4 5 6 8)
