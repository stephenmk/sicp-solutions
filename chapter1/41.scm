#| Exercise 1.41.

Define a procedure "double" that takes a procedure of one argument as
argument and returns a procedure that applies the original procedure
twice. For example, if inc is a procedure that adds 1 to its argument,
then (double inc) should be a procedure that adds 2. What value is
returned by

(((double (double double)) inc) 5)

|#

(define (double f)
  (lambda (x) (f (f x))))

(define inc 1+)

((double inc) 5) ; => 7

;; Applying the double transformation to itself, we get four
;; applications total of the function argument.
(define (quadruple f)
  (lambda (x) (((double double) f) x)))

((quadruple inc) 5) ; => 9

#| For "double quadruple," we apply the quadruple procedure to itself
twice. The first application of quadruple produces four applications
of the function argument. With the second application of quadruple,
these four function arguments are again quadrupled to produce 16
applications total.

The name for the 16-tuple is
sexdecuple. (https://en.wikipedia.org/wiki/Tuple) |#

(define (sexdecuple f)
  (lambda (x) (((double quadruple) f) x)))

;; The following are equivalent
(((double (double double)) inc) 5) ; => 21
(((double quadruple) inc) 5) ; => 21
((quadruple (quadruple inc)) 5) ; => 21
((sexdecuple inc) 5) ; => 21
(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))))))))) ; => 21
