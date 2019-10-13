#| Exercise 2.26

Suppose we define `x' and `y' to be two lists:

(define x (list 1 2 3))

(define y (list 4 5 6))

What result is printed by the interpreter in response to
evaluating each of the following expressions:

(append x y)

(cons x y)

(list x y)

|#


#| Solution

(append x y)
;; outputs combination into a flat list
(1 2 3 4 5 6)


(cons x y)
;; constructs a pair
;; must have (car (cons x y)) = x, and (cdr (cons x y)) = y
((1 2 3) 4 5 6)


(list x y)
;; by the definition of `list,' this will be the same as
;; (cons x (cons y '()))
;; so we need to have (car (list x y)) = x and (cdr (list x y)) = (y)
((1 2 3) (4 5 6))

|#

;; verify
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
;;Value: (1 2 3 4 5 6)

(cons x y)
;;Value: ((1 2 3) 4 5 6)

(list x y)
;;Value: ((1 2 3) (4 5 6))
