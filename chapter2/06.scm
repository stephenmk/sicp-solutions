#| *Exercise 2.6:*

In case representing pairs as procedures wasn't mind-boggling enough,
consider that, in a language that can manipulate procedures, we can
get by without numbers (at least insofar as nonnegative integers are
concerned) by implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

This representation is known as "Church numerals", after its
inventor, Alonzo Church, the logician who invented the [lambda]
calculus.

    Define `one' and `two' directly (not in terms of `zero' and
`add-1').  (Hint: Use substitution to evaluate `(add-1 zero)').  Give
a direct definition of the addition procedure `+' (not in terms of
repeated application of `add-1'). |#

#| Solution

Apply the substitution model
1.    (add-1 zero)
2.    (lambda (f) (lambda (x) (f ((zero f) x))))
3.    (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
4.    (lambda (f) (lambda (x) (f x))) |#

(define one (lambda (f) (lambda (x) (f x))))

#| Apply the substitution model again
1.    (add-1 one)
2.    (lambda (f) (lambda (x) (f ((one f) x))))
3.    (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
4.    (lambda (f) (lambda (x) (f (f x))))
|#

(define two (lambda (f) (lambda (x) (f (f x)))))

#| A Church numeral of a given number `a' is a lambda which applies a
function `f' to an argument `x' a number of times equal to the given
number `a.'

The addition of numerals `a' and `b' should therefore apply the
function `f' to argument `x' a number of times equal to `a', and then
a number of times equal to `b'. |#

(define (add a b)
  (lambda (f) (lambda (x) ((b f) ((a f) x)))))


(define (square x) (* x x))
((two square) 2) ; => 16 = (2^2)^2
(((add one two) square) 2) ; => 256 = ((2^2)^2)^2
(((add two two) square) 2) ; => 65536 = (((2^2)^2)^2)^2
(((add one (add two two)) square) 2) ; => 4294967296 = ((((2^2)^2)^2)^2)^2
