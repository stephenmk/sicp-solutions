#| Exercise 1.32.

a. Show that sum and product (exercise 1.31) are both special cases of
a still more general notion called accumulate that combines a
collection of terms, using some general accumulation function:

(accumulate combiner null-value term a next b)

Accumulate takes as arguments the same term and range specifications
as sum and product, together with a combiner procedure (of two
arguments) that specifies how the current term is to be combined with
the accumulation of the preceding terms and a null-value that
specifies what base value to use when the terms run out. Write
accumulate and show how sum and product can both be defined as simple
calls to accumulate.

b. If your accumulate procedure generates a recursive process, write
one that generates an iterative process. If it generates an iterative
process, write one that generates a recursive process. |#


#| Solution

The last exercise demonstrated how to transform the book's "sum"
procedure into a "product" procedure.  The operator (combiner) was
changed from + to *, and the identity term (null-value) was changed
from 0 to 1.  The accumulate procedures thus only need to adapt the
previously used sum and product procedures and accept the combiner and
null-value definitions as parameters. |#

;; Iterative accumulate procedure
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; Recursive accumulate procedure. Don't forget to add the combiner
;; and null-value parameters to the procedure call.
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))


;; Test sum using simpson procedure from exercise 1.29
(define (simpson f a b n)
  (define h
    (/ (- b a) n))
  (define (y k)
    (* (f (+ a (* k h)))
       (cond ((= k 0) 1)
             ((= k n) 1)
             ((= 0 (remainder k 2)) 2)
             (else 4))))
  (define (inc n)
    (+ n 1))
  (* (/ h 3)
     (sum y 0 inc n)))

(define (cube x)
  (* x x x))

(simpson cube 0 1 100) ; => 1/4


;; Test product using factorial procedure from previous exercise
(define (factorial n)
  (define (increment x)
    (+ x 1))
  (define (identity x) x)
  (product identity 1 increment n))

(factorial 2) ; => 2
(factorial 3) ; => 6
(factorial 4) ; => 24
(factorial 5) ; => 120
(factorial 6) ; => 720
