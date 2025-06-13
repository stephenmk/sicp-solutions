#| Exercise 2.57

Extend the differentiation program to handle sums and products of
arbitrary numbers of (two or more) terms. Then the last example above
could be expressed as

     (deriv '(* x y (+ x 3)) 'x)

Try to do this by changing only the representation for sums and
products, without changing the `deriv' procedure at all. For example,
the `addend' of a sum would be the first term, and the `augend' would
be the sum of the rest of the terms. |#

;;; Solution

;; Definitions from the previous exercise
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? expression num)
  (and (number? expression) (= expression num)))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))

;; Rewrite `make-sum' to accept variable number of arguments. Using
;; the `apply' procedure that I technically haven't learned yet. Note
;; that when I use `apply', I have to make sure I'm passing at least
;; two arguments to the `make-sum' procedure.
(define (make-sum a1 a2 . a3)
  (cond ((null? a3)
         (cond ((=number? a1 0) a2)
               ((=number? a2 0) a1)
               ((and (number? a1) (number? a2)) (+ a1 a2))
               (else (list '+ a1 a2))))
        ((null? (cdr a3))
         (make-sum (make-sum a1 a2) (car a3)))
        (else (make-sum (make-sum a1 a2)
                        (apply make-sum a3)))))

;; Rewriting `make-product' using the same strategy.
(define (make-product m1 m2 . m3)
  (cond ((null? m3)
         (cond ((or (=number? m1 0) (=number? m2 0)) 0)
               ((=number? m1 1) m2)
               ((=number? m2 1) m1)
               ((and (number? m1) (number? m2)) (* m1 m2))
               (else (list '* m1 m2))))
         ((null? (cdr m3))
          (make-product (make-product m1 m2) (car m3)))
         (else (make-product (make-product m1 m2)
                             (apply make-product m3)))))

;; Rewrite `augend' to account for extra terms.
(define (augend s)
  (let ((extra-terms (cddr s)))
    (if (null? (cdr extra-terms))
        (car extra-terms)
        (apply make-sum extra-terms))))

;; Make the same adjustment to `multiplicand' as I did to `augend'.
(define (multiplicand p)
  (let ((extra-terms (cddr p)))
    (if (null? (cdr extra-terms))
        (car extra-terms)
        (apply make-product extra-terms))))

;; Unaltered `deriv' definition from the book.
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; Tests
(make-sum 1 2)            ;Value: 3
(make-sum 1 2 3)          ;Value: 6
(make-sum 'x 'y)          ;Value: (+ x y)
(make-sum 'x 'y 'z)       ;Value: (+ (+ x y) z)
(make-sum 'a 'b 'c 'd)    ;Value: (+ (+ a b) (+ c d))
(make-sum 'a 'b 'c 'd 'e) ;Value: (+ (+ a b) (+ (+ c d) e))

(addend (make-sum 'x 'y)) ;Value: x
(augend (make-sum 'x 'y)) ;Value: y

(addend (make-sum 'x 'y 'z)) ;Value: (+ x y)
(augend (make-sum 'x 'y 'z)) ;Value: z

(make-product 1 2)            ;Value: 2
(make-product 1 2 3)          ;Value: 6
(make-product 'x 'y)          ;Value: (* x y)
(make-product 'x 'y 'z)       ;Value: (* (* x y) z)
(make-product 'a 'b 'c 'd)    ;Value: (* (* a b) (* c d))
(make-product 'a 'b 'c 'd 'e) ;Value: (* (* a b) (* (* c d) e))

(multiplier   (make-product 'x 'y)) ;Value: x
(multiplicand (make-product 'x 'y)) ;Value: y

(multiplier   (make-product 'x 'y 'z)) ;Value: (* x y)
(multiplicand (make-product 'x 'y 'z)) ;Value: z

(deriv '(* x y (+ x 3)) 'x) ;Value: (+ (* x y) (* y (+ x 3)))
