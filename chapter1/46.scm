#| Exercise 1.46.

Several of the numerical methods described in this chapter are
instances of an extremely general computational strategy known as
iterative improvement. Iterative improvement says that, to compute
something, we start with an initial guess for the answer, test if the
guess is good enough, and otherwise improve the guess and continue the
process using the improved guess as the new guess. Write a procedure
iterative-improve that takes two procedures as arguments: a method for
telling whether a guess is good enough and a method for improving a
guess. Iterative-improve should return as its value a procedure that
takes a guess as argument and keeps improving the guess until it is
good enough. Rewrite the sqrt procedure of section 1.1.7 and the
fixed-point procedure of section 1.3.3 in terms of
iterative-improve. |#

;; sqrt procedure definition from section 1.1.7
(define (sqrt x)
  (define (average a b) (/ (+ a b) 2.0))
  (define (square a) (* a a))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x)))
  (sqrt-iter 1.0 x))

;; extract the general structure into a generic procedure
(define (iterative-improve good-enough? improve)
  (lambda (initial-guess)
    (define (try guess)
      (if (good-enough? guess)
          guess
          (try (improve guess))))
    (try initial-guess)))

(define (sqrt x)
  (define (average a b) (/ (+ a b) 2.0))
  (define (square a) (* a a))
  ((iterative-improve (lambda (g) (< (abs (- (square g) x)) 0.001))
                      (lambda (g) (average g (/ x g))))
   1.0))

(sqrt 400.0) ; => 20.000000000298428



;; fixed-point procedure definition from 1.3.3
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

;; have to calculate (f g) twice as often this way, unfortunately
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  ((iterative-improve (lambda (g) (< (abs (- g (f g))) tolerance))
                      (lambda (g) (f g)))
   first-guess))

;; fixed point of y = sin(y) + cos(y), from book
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0) ; => 1.2587228743052672
