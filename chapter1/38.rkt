(require sicp)

;;; Exercise 1.38.
;; In 1737, the Swiss mathematician Leonhard Euler published a memoir
;; De Fractionibus Continuis, which included a continued fraction
;; expansion for e - 2, where e is the base of the natural
;; logarithms. In this fraction, the N_i are all 1, and the D_i are
;; successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program
;; that uses your cont-frac procedure from exercise 1.37 to
;; approximate e, based on Euler's expansion.

;;; Solution

;; iterative cont-frac definition from previous exercise
(define (cont-frac n d k)
  (define (cont-frac-iterative i v)
    (if (= i 0)
        v
        (cont-frac-iterative (- i 1)
                             (/ (n i) (+ (d i) v)))))
  (cont-frac-iterative k 0))

;; When i+1 is not divisible by 3, the corresponding D_i value is 1.
;; Otherwise, the value is divisible by three, then the corresponding
;; D_i value is equal to the number of times it is divisible by 3,
;; multiplied by 2.
(define (euler-sequence i)
  (if (= 0 (modulo (+ i 1) 3))
      (* 2 (quotient (+ i 1) 3))
      1))

(define (estimate-euler-constant k)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  euler-sequence
                  k)))

(estimate-euler-constant 1000) ; => 2.7182818284590455
