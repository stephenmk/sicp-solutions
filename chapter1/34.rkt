(require sicp)

;;; Exercise 1.34.
;; Suppose we define the procedure

(define (f g)
  (g 2))

;; Then we have

(f square)
4

(f (lambda (z) (* z (+ z 1))))
6

;; What happens if we (perversely) ask the interpreter to evaluate the
;; combination (f f)? Explain.

;;; Solution
;; Use the substituion model.
;; Replace f with a lambda procedure equivalent of f.

(f f)
(f (lambda (g) (g 2)))
((lambda (g) (g 2)) 2)
(2 2)

;; The interpreter attempts to evaluate (2 2), treating the primitive
;; element 2 as a procedure call. This produces an error.
