(require sicp)

;; Exercise 1.11
;; A function f is defined by the rule that f(n) = n if n < 3 and
;; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3. Write a procedure
;; that computes f by means of a recursive process. Write a procedure
;; that computes f by means of an iterative process

;; recursive process -- just adapt function definition
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

;; example
;: (f 4)
;: (+ (f 3) (* 2 (f 2)) (* 3 (f 1)))
;: (+ (+ (f 2) (* 2 (f 1)) (* 3 (f 0))) (* 2 (f 2)) (* 3 (f 1)))
;: (+ (+ 2 (* 2 1) (* 3 0)) (* 2 2) (* 3 1))
;: (+ (+ 2 2 0) 4 3)
;: (+ 4 4 3)
;: 11

;: (f 20) ; => 10771211

;; iterative process
;; Four state variables: one counter and three previously calculated values
(define (f n)
  (define (f-iter step v1 v2 v3)
    (cond ((< n 3) n)
          ((< n step) v1)
          (else (f-iter (+ step 1)
                        (+ v1 (* 2 v2) (* 3 v3))
                        v1
                        v2))))
  (f-iter 3 2 1 0))

;: (f 4) ; => 11
;: (f 20) ; => 10771211
