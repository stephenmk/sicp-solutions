#| Exercise 1.16.

Design a procedure that evolves an iterative exponentiation process
that uses successive squaring and uses a logarithmic number of steps,
as does fast-expt. (Hint: Using the observation that (b^(n/2))^2
= (b^2)^(n/2), keep, along with the exponent n and the base b, an
additional state variable a, and define the state transformation in
such a way that the product (a b^n) is unchanged from state to
state. At the beginning of the process a is taken to be 1, and the
answer is given by the value of a at the end of the process. In
general, the technique of defining an invariant quantity that remains
unchanged from state to state is a powerful way to think about the
design of iterative algorithms.)

|#

;; Recursive version from book
(define (fast-expt b n)
    (define (even? n)
      (= (remainder n 2) 0))

    (define (square x) (* x x))

    (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(fast-expt 4 16) ; => 4294967296

#| Solution

The general idea is that any exponent can be broken-down into its
base-2 representation. For example, 37 = 32 + 4 + 1.
And so b^37= (b^32)(b^4)(b^1).

The strategy for designing this procedure is therefore
to keep computing the squares until the exponent value is
reached.

Every time an odd exponent is reached, the state variable
is multiplied by the currently calculated square.
|#

;; Iterative version
(define (fast-expt b n)
  (define (even? x)
    (= (remainder x 2) 0))

  (define (square x) (* x x))

  (define (fast-expt-iter x y z)
    (cond ((= y 0) z)
          ((even? y) (fast-expt-iter (square x) (/ y 2) z)) ; Continue computing squares until n is not even
          (else (fast-expt-iter x (- y 1) (* x z))))) ; Multiply state variable by factor and decrement n

  (fast-expt-iter b n 1.0))

(fast-expt 4 16) ; => 4294967296

#|

|----------+--------+----------------|
| exponent | square | state variable |
|----------+--------+----------------|
|       37 | b      | 1              |
|       36 | b      | b              |
|       18 | b^2    | b              |
|        9 | b^4    | b              |
|        8 | b^4    | b(b^4)         |
|        4 | b^8    | b(b^4)         |
|        2 | b^16   | b(b^4)         |
|        1 | b^32   | b(b^4)         |
|        0 | b^32   | b(b^4)(b^32)   |
|----------+--------+----------------|

|#
