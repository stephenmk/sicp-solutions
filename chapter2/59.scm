#| Exercise 2.59

Implement the `union-set' operation for the unordered-list
representation of sets. |#

;; Definitions from book
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;; Solution
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1)
                         (adjoin-set (car set1) set2)))))

;;; Tests
(union-set '() '(1 2))         ;Value: (1 2)
(union-set '(1 2) '())         ;Value: (1 2)
(union-set '(1 2) '(1 2))      ;Value: (1 2)
(union-set '(1 2) '(3 4))      ;Value: (2 1 3 4)
(union-set '(1 2) '(1 2 3 4))  ;Value: (1 2 3 4)
(union-set '(3 4) '(1 2 3 4))  ;Value: (1 2 3 4)
(union-set '(1 3) '(1 2 3 4))  ;Value: (1 2 3 4)
(union-set '(a b) '(a b c d))  ;Value: (a b c d)
