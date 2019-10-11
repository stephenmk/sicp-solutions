#| Exercise 2.7

Alyssa's program is incomplete because she has not specified the
implementation of the interval abstraction.  Here is a definition of
the interval constructor:

(define (make-interval a b) (cons a b))

Define selectors `upper-bound' and `lower-bound' to complete the
implementation. |#

(define (make-interval a b) (cons a b))

;; Naive implementation: assume bounds are ordered lower to upper
(define upper-bound cdr)
(define lower-bound car)

;; More robust implementation: compare values to determine upper and lower
(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))


(lower-bound (make-interval 20 -5)) ;Value: -5
