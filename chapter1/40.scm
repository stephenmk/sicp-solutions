#| Exercise 1.40.

Define a procedure "cubic" that can be used together with the
"newtons-method" procedure in expressions of the form

(newtons-method (cubic a b c) 1)

to approximate zeros of the cubic x^3 + ax^2 + bx + c. |#


;; fixed-point definition from book
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; newtons-method definition from book
(define (newtons-method f guess)
  (define dx 0.00001)
  (define (deriv g)
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
         dx)))
  (define (newton-transform g)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x)))))
  (fixed-point (newton-transform f) guess))

;; cubic must accept three parameters and return a function
;; x -> x^3 + ax^2 + bx + c
(define (cubic a b c)
  (define (square x) (* x x))
  (define (cube x) (* x x x))
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

;; A contrived example: a=2,b=2,c=1 has a zero at x = -1.
;; x^3 + ax^2 + bx + c = (-1)^3 + 2(-1)^2 + 2(-1) + 1
;;                     = -1 + 2 - 2 + 1
;;                     = 0

(newtons-method (cubic 2 2 1) 1) ; => -0.9999999999849342
