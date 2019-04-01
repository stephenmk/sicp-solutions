(require sicp)

;;; Exercise 1.35.
;; Show that the golden ratio φ (section 1.2.2) is a fixed point of
;; the transformation x -> 1 + 1/x, and use this fact to compute by
;; means of the fixed-point procedure.

;;; Solution
;; By definition, φ = (1 + √5) / 2
;; So for f(x) = 1 + 1/x, we must show f(φ) = φ.
;; f(φ) = 1 + 1/φ
;;      = 1 + 2/(1 + √5)
;;      = (1 + √5)/(1 + √5) + 2/(1 + √5)
;;      = (1 + √5 + 2)/(1 + √5)
;;      = (3 + √5)/(1+√5)
;;      = (1/2)(6 + 2√5)/(1+√5)
;;      = (1/2)(1 + 2√5 + 5)/(1+√5)
;;      = (1/2)(1+√5)(1+√5)/(1+√5)
;;      = (1+√5)/2
;;      = φ

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

(fixed-point (lambda (x) (+ 1 (/ 1 x)) ) 1.0) ; => 1.6180327868852458
