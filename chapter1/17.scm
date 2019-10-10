#| Exercise 1.17.

The exponentiation algorithms in this section are based on
performing exponentiation by means of repeated multiplication. In a
similar way, one can perform integer multiplication by means of
repeated addition. The following multiplication procedure (in which
it is assumed that our language can only add, not multiply) is
analogous to the expt procedure:

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

This algorithm takes a number of steps that is linear in b. Now
suppose we include, together with addition, operations double, which
doubles an integer, and halve, which divides an (even) integer by
2. Using these, design a multiplication procedure analogous to
fast-expt that uses a logarithmic number of steps.

|#

#| Solution

Halve a and double b continuously until a is equal to 1.  We can do
this because ab = (a/2)(b*2).  When a is not even, I make use of the
fact that ab = ((a-1)+1)+b = ((a-1)b) + b in order to make the first
argument an even number.  I also account for special situations in
which the arguments are negative, equal to 0, or equal to 1.

|#

(define (fast-multiply a b)
  (define (double x)
    (* x 2))

  (define (halve x)
    (/ x 2))

  (define (even? x)
    (= (remainder x 2) 0))

  (cond
        ;; Check for null arguments
        ((= a 0) 0)
        ((= b 0) 0)

        ;; Check for multiplicative identity (1)
        ((= a 1) b)
        ((= b 1) a)

        ;; Check for negative arguments
        ((and (< a 0) (< b 0)) (fast-multiply (abs a) (abs b)))
        ((< a 0) (fast-multiply (abs a) (* b -1)))

        ;; Apply the formulas described above
        ;; Note that the first argument must be positive
        ((even? a) (fast-multiply (halve a) (double b)))
        (else (+ b (fast-multiply (- a 1) b)))
        ))

(fast-multiply  5  21) ; =>  105
(fast-multiply -5  21) ; => -105
(fast-multiply  5 -21) ; => -105
(fast-multiply -5 -21) ; =>  105
