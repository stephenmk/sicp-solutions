#| Exercise 1.20.

The process that a procedure generates is of course dependent on the
rules used by the interpreter. As an example, consider the iterative
gcd procedure given above

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

Suppose we were to interpret this procedure using normal-order
evaluation, as discussed in section 1.1.5. (The
normal-order-evaluation rule for if is described in exercise 1.5.)
Using the substitution method (for normal order), illustrate the
process generated in evaluating (gcd 206 40) and indicate the
remainder operations that are actually performed. How many remainder
operations are actually performed in the normal-order evaluation
of (gcd 206 40)? In the applicative-order evaluation?

|#

#| Solution

Applicative order evaluation requires much fewer evaluations of the
remainder procedure. They both take the same number of steps, but
normal order also requires many deferred operations to remember.

|#

;; Applicative order evaluation
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6) ; 1st evaluation
(gcd 6 (remainder 40 6))
(gcd 6 4) ; 2nd evaluation
(gcd 4 (remainder 6 4))
(gcd 4 2) ; 3rd evaluation
(gcd 2 (remainder 4 2))
(gcd 2 0) ; 4th evaluation
2

;; Normal-order evaluation
(gcd 206 40)
(gcd 40 (remainder 206 40))

(if (= (remainder 206 40) 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

; 1st evaluation of remainder
(if (= 6 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))

(if (= (remainder 40 (remainder 206 40)) 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

;; 2 more evaluations of remainder (3 total)
(if (= 4 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))

(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

;; 4 more evaluations of remainder (7 total)
(if (= 2 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))

;; 7 more evaluations of remainder (14 total)
(if (= 0 0)
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))

(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))

;; 4 more evalutions (18 total)
2
