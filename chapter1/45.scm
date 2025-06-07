#| Exercise 1.45

We saw in section 1.3.3 that attempting to compute square roots by
naively finding a fixed point of y |-> x/y does not converge, and that
this can be fixed by average damping. The same method works for
finding cube roots as fixed points of the average-damped y |-> x/y^2.
Unfortunately, the process does not work for fourth roots -- a single
average damp is not enough to make a fixed-point search for
y |-> x/y^3 converge. On the other hand, if we average damp twice
(i.e., use the average damp of the average damp of y |-> x/y^3) the
fixed-point search does converge. Do some experiments to determine how
many average damps are required to compute nth roots as a fixed-point
search based upon repeated average damping of y |-> x/y^(n-1).  Use
this to implement a simple procedure for computing nth roots using
`fixed-point', `average-damp', and the `repeated' procedure of
exercise 1.43. Assume that any arithmetic operations you need are
available as primitives. |#

;;; Solution

;; `fixed-point' procedure from section 1.3.3
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

;; `average-damp' procedure from section 1.3.4
(define (average-damp f)
  (define (average x y)
    (/ (+ x y) 2))
  (lambda (x) (average x (f x))))

;; `repeated' procedure from exercise 1.43
(define (repeated f n)
  (define (compose f g)
    (lambda (x) (f (g x))))
  (define (repeated-iter g i)
    (if (= i n)
        g
        (repeated-iter (compose f g) (+ i 1))))
  (repeated-iter f 1))

;; `sqrt' procedure from section 1.3.4
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(sqrt 4)  ;Value: 2.000000000000002
(sqrt 16) ;Value: 4.000000000000051

;; `cube-root' procedure from section 1.3.4
(define (cube-root x)
  (define (square x) (* x x))
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(cube-root 8)  ;Value: 1.9999981824788517
(cube-root 27) ;Value: 2.9999972321057697

;; For my experiment, I'll raise a big number (451) to the nth power
;; and attempt to find the nth root of it (should be
;; approx. 451). I'll keep raising n until the number of average
;; dampings is insufficient for convergence.

(define (nth-root-tester x n dampings)
  (define (next-damper n)
    (repeated average-damp dampings))
  (let ((x-to-the-n (expt x n)))
    (fixed-point ((next-damper n) (lambda (y) (/ x-to-the-n (expt y (- n 1)))))
                 1.0)))

(nth-root-tester 451 2 1) ; Value: 451.
(nth-root-tester 451 3 1) ; Value: 451.0000016728738
;;(nth-root-tester 451 4 1) ; Doesn't converge

(nth-root-tester 451 4 2) ; Value: 451.
(nth-root-tester 451 5 2) ; Value: 450.99999945947263
(nth-root-tester 451 6 2) ; Value: 451.00000239345695
(nth-root-tester 451 7 2) ; Value: 450.9999957537591
;;(nth-root-tester 451 8 2) ; Doesn't converge

(nth-root-tester 451 8 3)  ; Value: 451.
(nth-root-tester 451 9 3)  ; Value: 450.9999991936219
(nth-root-tester 451 10 3) ; Value: 450.99999935005076
(nth-root-tester 451 11 3) ; Value: 450.99999759038974
(nth-root-tester 451 12 3) ; Value: 451.0000017596527
(nth-root-tester 451 13 3) ; Value: 450.9999965824627
(nth-root-tester 451 14 3) ; Value: 450.9999967374824
(nth-root-tester 451 15 3) ; Value: 450.9999958964655
;;(nth-root-tester 451 16 3) ; Doesn't converge

(nth-root-tester 451 16 4) ; Value: 451.
(nth-root-tester 451 31 4) ; Value: 451.00000472780687
;;(nth-root-tester 451 32 4) ; Doesn't Converge

(nth-root-tester 451 32 5) ; Value: 451.
(nth-root-tester 451 63 5) ; Value: 451.0000047885947
;;(nth-root-tester 451 64 5) ; Doesn't converge

(nth-root-tester 451 64 6) ; Value: 451.
(nth-root-tester 20 127 6) ; Value: 20.000004912105517
;; (nth-root-tester 20 128 6) ; Doesn't converge
(nth-root-tester 20 128 7) ; Value: 20.

#| The experiments show that the number of required average dampings
increases by one at every power of 2, i.e. the 8th root, 16th root,
32nd root, 64th root, etc.

In other words, the required number of average dampings increases with
the logarithm of 2.

Since MIT-Scheme only includes the natural log (base e) as a
primitive, I'll need to use the change-of-base formula to compute log
base 2. [log_2(x) = log(x) / log(2)] |#

(define (nth-root x n)
  (let ((dampings (floor (/ (log n) (log 2)))))
    (define (next-damper n)
      (repeated average-damp dampings))
    (fixed-point ((next-damper n) (lambda (y) (/ x (expt y (- n 1)))))
                 1.0)))

(nth-root 27 3)              ; Value: 2.9999972321057697
(nth-root (expt 451 64) 64)  ; Value: 451.
(nth-root (expt 20 127) 127) ; Value: 20.000004912105517
(nth-root (expt 20 128) 128) ; Value: 20.
