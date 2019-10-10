#| Exercise 2.4

Here is an alternative procedural representation of pairs.  For this
representation, verify that `(car (cons x y))' yields `x' for any
objects `x' and `y'.

          (define (cons x y)
            (lambda (m) (m x y)))

          (define (car z)
            (z (lambda (p q) p)))

What is the corresponding definition of `cdr'? (Hint: To verify that
this works, make use of the substitution model of section 1.1.5.)

|#

#| Verification of car using substitution model

1.    (car (cons x y))
2.    (car (lambda (m) (m x y)))
3.    ((lambda (m) (m x y)) ((lambda (p q) p)))
4.    ((lambda (p q) p) x y)
5.    x

|#

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(car (cons 'x 'y)) ; => x

;;; To get cdr definition, modify the car definition to return the
;;; second parameter instead of the first.

(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons 'x 'y)) ; => y
