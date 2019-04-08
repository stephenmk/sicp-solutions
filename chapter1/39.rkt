(require sicp)

;;; Exercise 1.39.
;; A continued fraction representation of the tangent function was
;; published in 1770 by the German mathematician J.H. Lambert:
;;
;; tan x = x / (1 - (x^2 / (3 - (x^2 / (5 - ...)))))
;;
;; where x is in radians. Define a procedure (tan-cf x k) that
;; computes an approximation to the tangent function based on
;; Lambert's formula. K specifies the number of terms to compute, as
;; in exercise 1.37.

;;; Solution

;; iterative cont-frac definition from exercise 1.37
(define (cont-frac n d k)
  (define (cont-frac-iterative i v)
    (if (= i 0)
        v
        (cont-frac-iterative (- i 1)
                             (/ (n i) (+ (d i) v)))))
  (cont-frac-iterative k 0))

;; Here the N_i terms are equal to x for i=1, and equal to -x^2 for all
;; other i. The D_i terms are a sequence of increasing odd numbers.

(define (tan-cf x k)
  (define (square x) (* x x))
  (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
             (lambda (i) (- (* i 2) 1))
             k))

(tan-cf 1.0 1000) ; => 1.557407724654902

;; How to get a sequence of odd numbers: add each number to itself, minus 1.
;; 1  = 1 + 0
;; 3  = 2 + 1
;; 5  = 3 + 2
;; 7  = 4 + 3
;; 9  = 5 + 4
;; 11 = 6 + 5
;; 13 = 7 + 6
;; 15 = 8 + 7
;; 17 = 9 + 8
