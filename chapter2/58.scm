#| Exercise 2.58

Suppose we want to modify the differentiation program so that it works
with ordinary mathematical notation, in which `+' and `*' are infix
rather than prefix operators. Since the differentiation program is
defined in terms of abstract data, we can modify it to work with
different representations of expressions solely by changing the
predicates, selectors, and constructors that define the representation
of the algebraic expressions on which the differentiator is to
operate.

a. Show how to do this in order to differentiate algebraic expressions
presented in infix form, such as `(x + (3 * (x + (y + 2))))'. To
simplify the task, assume that `+' and `*' always take two arguments
and that expressions are fully parenthesized.

b. The problem becomes substantially harder if we allow standard
algebraic notation, such as `(x + 3 * (x + y + 2))', which drops
unnecessary parentheses and assumes that multiplication is done before
addition.  Can you design appropriate predicates, selectors, and
constructors for this notation such that our derivative program still
works? |#

;;; Solution (a)

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? expression num)
  (and (number? expression) (= expression num)))

;; Move the '+ symbol to the middle of the list.
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

;; Move the '* symbol to the middle of the list.
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;; Shuffle the `cadr' and `car' usages to account for the new list
;; order.
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

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

;;; Tests
(make-sum 'x 'y)     ;Value: (x + y)
(make-product 'x 'y) ;Value: (x * y)

(addend (make-sum 'x 'y)) ;Value: x
(augend (make-sum 'x 'y)) ;Value: y

(multiplier   (make-product 'x 'y)) ;Value: x
(multiplicand (make-product 'x 'y)) ;Value: y

(deriv '((x * y) * (x + 3)) 'x) ;Value: ((x * y) + (y * (x + 3)))


;;; Solution (b)

;; Since addition always has lower precedence over multiplication, we can
;; treat a symbolic expression such as `(x + 3 * (x + y + 2))' as a sum
;; with addend `x' and augend `3 * (x + y + 2)'.

(define (sum? x)
  "Would rather return #t or #f explicitly; don't want result of MEMQ."
  (if (and (pair? x) (memq '+ x)) #t #f))

(define (addend s)
  (define (addend-list sl)
    (cond ((null? sl) '())
          ((equal? (memq '+ sl) (cdr sl)) (list (car sl)))
          (else (cons (car sl) (addend-list (cdr sl))))))
  (if (equal? (memq '+ s) (cdr s))
      (car s)
      (addend-list s)))

(define (augend s)
  (let ((terms (cdr (memq '+ s))))
    (if (null? (cdr terms))
        (car terms)
        terms)))

(define (product? x)
  (and (pair? x)
       (memq '* x)
       (not (sum? x))))

(define (multiplier p)
  (define (multiplier-list pl)
    (cond ((null? pl) '())
          ((equal? (memq '* pl) (cdr pl)) (list (car pl)))
          (else (cons (car pl) (multiplier-list (cdr pl))))))
  (if (equal? (memq '* p) (cdr p))
      (car p)
      (multiplier-list p)))

(define (multiplicand p)
  (let ((terms (cdr (memq '* p))))
    (if (null? (cdr terms))
        (car terms)
        terms)))

;;; Tests

(sum? '(x + y))       ;Value: #t
(sum? '(x + y + z))   ;Value: #t
(sum? '(1 + x * y))   ;Value: #t
(sum? '(x * y + 1))   ;Value: #t
(sum? '(x * (y + 1))) ;Value: #f
(sum? '((x + y) * 2)) ;Value: #f

(addend '(x + y))     ;Value: x
(augend '(x + y))     ;Value: y
(addend '(x + y + z)) ;Value: x
(augend '(x + y + z)) ;Value: (y + z)

(product? '(x * y))       ;Value: #t
(product? '(x * y * z))   ;Value: #t
(product? '(1 + x * y))   ;Value: #f
(product? '(x * y + 1))   ;Value: #f
(product? '((1 + x) * y)) ;Value: #t
(product? '((x * y) + 1)) ;Value: #f

(multiplier   '(x * y))     ;Value: x
(multiplicand '(x * y))     ;Value: y
(multiplier   '(x * y * z)) ;Value: x
(multiplicand '(x * y * z)) ;Value: (y * z)

(deriv '(x * y * (x + 3)) 'x) ;Value: ((x * y) + (y * (x + 3)))
