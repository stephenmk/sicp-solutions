#| Exercise 1.29.

Simpson's Rule is a more accurate method of numerical integration than
the method illustrated above. Using Simpson's Rule, the integral of a
function f between a and b is approximated as

(h/3) [y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ⋯ + 2y_(n−2) + 4y_(n−1) + y_n]

where h = (b−a)/n, for some even integer n, and
y_k=f(a+kh). (Increasing n increases the accuracy of the
approximation.) Define a procedure that takes as arguments f, a, b,
and n and returns the value of the integral, computed using Simpson's
Rule. Use your procedure to integrate cube between 0 and 1 (with n =
100 and n = 1000), and compare the results to those of the integral
procedure shown above. |#

;;; Solution

;; Definition of "sum" from book
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (define h
    (/ (- b a) n))
  (define (y k)
    (* (f (+ a (* k h)))
       (cond ((= k 0) 1)
             ((= k n) 1)
             ((= 0 (remainder k 2)) 2)
             (else 4))))
  (define (inc n)
    (+ n 1))
  (* (/ h 3)
     (sum y 0 inc n)))

(define (cube x)
  (* x x x))

(simpson cube 0 1 100) ; => 1/4
(simpson cube 0 1 1000) ; => 1/4

#| Procedure returns the exact value of the defined integral. Value is
the same result found from the integral procedure shown in the
book. |#
