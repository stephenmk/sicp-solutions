#| Exercise 1.31

a. The sum procedure is only the simplest of a vast number of similar
abstractions that can be captured as higher-order procedures. Write an
analogous procedure called product that returns the product of the
values of a function at points over a given range. Show how to define
factorial in terms of product. Also use product to compute
approximations to using the formula

π/4 = (2 * 4 * 4 * 6 * 6 * 8 * ...) / (3 * 3 * 5 * 5 * 7 * 7 * ...)

b. If your product procedure generates a recursive process, write one
that generates an iterative process. If it generates an iterative
process, write one that generates a recursive process. |#


#| Solution Part A

Just change the + to a * in the procedure definition of sum. And
change the initial "result" value from 0, the additive identity, to 1,
the multiplicative identity. Otherwise, the result will always be
0. |#

(define (product term a next b)
  (define multiplicative-identity 1.0)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a multiplicative-identity))

(define (factorial n)
  (define increment 1+)
  (define (identity x) x)
  (product identity 1 increment n))

(factorial 2) ; => 2
(factorial 3) ; => 6
(factorial 4) ; => 24
(factorial 5) ; => 120
(factorial 6) ; => 720

(define (estimate-pi precision)
  (define increment 1+)
  (define (pi-term x)
    (if (= 0 (remainder x 2))
        (/ x (increment x))
        (/ (increment x) x)))
  (* 4 (product pi-term 2 increment precision)))

(estimate-pi 100000) ;Value: 3.1415769458226377

#| Solution Part B

Adjust the book's definition of recursive sum to use the * operator
instead of the + operator. Use the multiplicative identity instead of
the additive identity as well. |#

(define (product term a next b)
  (define multiplicative-identity 1.0)
  (if (> a b)
      multiplicative-identity
      (* (term a)
         (product term (next a) next b))))

(factorial 6) ;Value: 720.
(estimate-pi 100000) ;Value: 3.141576945822669
