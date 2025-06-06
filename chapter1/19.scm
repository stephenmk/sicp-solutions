#| Exercise 1.19

There is a clever algorithm for computing the Fibonacci numbers in a
logarithmic number of steps. Recall the transformation of the state
variables a and b in the `fib-iter' process of section 1.2.2:

    a <- a + b
    and
    b <- a.

Call this transformation T, and observe that applying T over and over
again n times, starting with 1 and 0, produces the pair Fib(n+1) and
Fib(n).  In other words, the Fibonacci numbers are produced by
applying T^n, the nth power of the transformation T, starting with the
pair (1,0).  Now consider T to be the special case of p=0 and q=1 in a
family of transformations T_(pq), where T_(pq) transforms the
pair (a,b) according to

    a <- bq + aq + ap
    and
    b <- bp + aq.

Show that if we apply such a transformation T_(pq) twice, the effect
is the same as using a single transformation T_(p'q') of the same
form, and compute p' and q' in terms of p and q. This gives us an
explicit way to square these transformations, and thus we can compute
T^n using successive squaring, as in the `fast-expt' procedure. Put
this all together to complete the following procedure, which runs in a
logarithmic number of steps:

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   <??>      ; compute p'
                   <??>      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
|#

;;; Solution

;; T_(p',q')f[a,b] = T_(p,q)T_(p,q)f[a,b]
;;                 = T_(p,q)f[bq + aq + ap, bp + aq]
;;                 = f[(bp+aq)q + (bq+aq+ap)q + (bq+aq+ap)p,
;;                     (bp+aq)p + (bq+aq+ap)q]
;;                 = f[bpq + aq² + bq² + aq² + apq + bpq + apq + ap²,
;;                     bp² + apq + bq² + aq² + apq]
;;                 = f[b(pq + q² + qp) + a(q² + q² + pq + pq + p²),
;;                     b(p² + q²) + a(pq + q² + pq)]
;;                 = f[b(q² + 2pq) + a(q² + 2pq) + a(p² + q²),
;;                     b(p² + q²) + a(q² + 2pq)]
;;
;; Let p' = p² + q² and q' = q² + 2pq. Then as desired,
;;
;; T_(p',q')f[a,b] = f[bq' + aq' + ap', bp' + aq'].

(define (fib n)
  (define (square x) (* x x))

  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))  ; compute p'
                     (+ (square q) (* 2 p q))   ; compute q'
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))

;;; Tests

;; ]=> (fib 5)  ;Value: 5
;; ]=> (fib 6)  ;Value: 8
;; ]=> (fib 7)  ;Value: 13
;; ]=> (fib 8)  ;Value: 21
;; ]=> (fib 9)  ;Value: 34
;; ]=> (fib 10) ;Value: 55
;; ]=> (fib 11) ;Value: 89
;; ]=> (fib 12) ;Value: 144
