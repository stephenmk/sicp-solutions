#| Exercise 2.73

Section 2.3.2 described a program that performs symbolic
differentiation:

    (define (deriv exp var)
      (cond ((number? exp) 0)
            ((variable? exp)
             (if (same-variable? exp var) 1 0))
            ((sum? exp)
             (make-sum (deriv (addend exp) var)
                       (deriv (augend exp) var)))
            ((product? exp)
             (make-sum (make-product
                        (multiplier exp)
                        (deriv (multiplicand exp) var))
                       (make-product
                        (deriv (multiplier exp) var)
                        (multiplicand exp))))
            ⟨more rules can be added here⟩
            (else (error "unknown expression type: DERIV" exp))))

We can regard this program as performing a dispatch on the type of the
expression to be differentiated. In this situation the “type tag” of
the datum is the algebraic operator symbol (such as +) and the
operation being performed is `deriv'. We can transform this program
into data-directed style by rewriting the basic derivative procedure
as |#

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

#| a. Explain what was done above. Why can’t we assimilate the predicates
`number?' and `variable?' into the data-directed dispatch? |#

;;; Solution

;; The new `operator' procedure checks the first item in the list of
;; an expression and uses it to determine which derivative procedure
;; should be applied. The `operands' procedure supplies the rest of
;; the expression to the derivative procedure. We can't directly
;; assimilate the `number?' and `variable?' predicates here because
;; the format assumes a list in which the car is the operator and the
;; cdr are the operands. Number and variable expressions will be
;; single items rather than a list, so applying `car' or `cdr' would
;; cause an error.

#| b. Write the procedures for derivatives of sums and products, and the
auxiliary code required to install them in the table used by the
program above. |#

;;; Solution

;; MIT-Scheme doesn't seem to have `get' and `put' implementations
;; by default, although the book assumes that it does. I'm borrowing
;; implementations from here:
;; https://stackoverflow.com/questions/5499005
(define *op-table* (make-hash-table))
(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))
(define (get op type)
  (hash-table/get *op-table* (list op type) #f))

;; Procedures from exercise 2.56
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? expression num)
  (and (number? expression) (= expression num)))
(define (addend s) (car s))
(define (augend s) (cadr s))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (multiplier p) (car p))
(define (multiplicand p) (cadr p))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;; New procedure package
(define (install-sum-product-deriv-package)
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  (define (deriv-product operands var)
    (make-sum (make-product
               (multiplier operands)
               (deriv (multiplicand operands) var))
              (make-product
               (deriv (multiplier operands) var)
               (multiplicand operands))))
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product))

(install-sum-product-deriv-package)

;; Test: (yx + x + 451), expect (y + 1)
(deriv '(+ (+ (* y x) x) 451) 'x)
;; Value: (+ y 1)


#| c. Choose any additional differentiation rule that you like, such as
the one for exponents (Exercise 2.56), and install it in this
data-directed system. |#

;;; Solution

(define (base e) (car e))
(define (exponent e) (cadr e))
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

(define (install-exponentiation-deriv-package)
  (define (deriv-exponent operands var)
    (make-product (exponent operands)
                  (make-product (make-exponentiation (base operands) (- (exponent operands) 1))
                                (deriv (base operands) var))))
  (put 'deriv '** deriv-exponent))

(install-exponentiation-deriv-package)

;; Test: (yx^5 + x^4 + 451), expect (5yx^4 + 4x^3)
(deriv '(+ (+ (* y (** x 5)) (** x 4)) 451) 'x)
;; Value: (+ (* y (* 5 (** x 4))) (* 4 (** x 3)))


#| d. In this simple algebraic manipulator the type of an expression is
the algebraic operator that binds it together. Suppose, however, we
indexed the procedures in the opposite way, so that the dispatch line
in `deriv' looked like

    ((get (operator exp) 'deriv) (operands exp) var)

What corresponding changes to the derivative system are required? |#

;;; Solution

;; The order in which the `put' procedure indexes the procedure would
;; simply need to be reversed.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv)
               (operands exp) var))))

(define (install-sum-product-deriv-package)
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  (define (deriv-product operands var)
    (make-sum (make-product
               (multiplier operands)
               (deriv (multiplicand operands) var))
              (make-product
               (deriv (multiplier operands) var)
               (multiplicand operands))))
  (put '+ 'deriv deriv-sum)
  (put '* 'deriv deriv-product))

(install-sum-product-deriv-package)

;; Test: (yx + x + 451), expect (y + 1)
(deriv '(+ (+ (* y x) x) 451) 'x)
;; Value: (+ y 1)
