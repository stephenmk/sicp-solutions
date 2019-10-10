#| Exercise 1.27

Demonstrate that the Carmichael numbers listed in footnote 47
really do fool the Fermat test. That is, write a procedure that
takes an integer n and tests whether a^n is congruent to a modulo n
for every a<n, and try your procedure on the given Carmichael
numbers. |#

;; Procedure for computing base^exp modulo m (from book)
(define (expmod base exp m)
  (define (square x) (* x x))
  (define (even? n) (= 0 (remainder n 2)))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;;; Solution
(define (carmichael-check n)
  (define (charmichael-check-iter a)
    (if (= a n)
        -1 ; No incongruent pairs found
        (if (= (expmod a n n) (remainder a n))
            (charmichael-check-iter (+ a 1))
            a ; Incongruent pair found at a
            )))
  (charmichael-check-iter 0))

(carmichael-check 561) ; => -1
(carmichael-check 1105) ; => -1
(carmichael-check 1729) ; => -1
(carmichael-check 2465) ; => -1
(carmichael-check 2821) ; => -1
(carmichael-check 6601) ; => -1
(carmichael-check 6602) ; => 2
