(require sicp)

;;; Exercise 1.33
;; You can obtain an even more general version of accumulate (exercise
;; 1.32) by introducing the notion of a filter on the terms to be
;; combined. That is, combine only those terms derived from values in
;; the range that satisfy a specified condition. The resulting
;; filtered-accumulate abstraction takes the same arguments as
;; accumulate, together with an additional predicate of one argument
;; that specifies the filter. Write filtered-accumulate as a
;; procedure. Show how to express the following using
;; filtered-accumulate:

;; a. the sum of the squares of the prime numbers in the interval a to
;; b (assuming that you have a prime? predicate already written)

;; b. the product of all the positive integers less than n that are
;; relatively prime to n (i.e., all positive integers i < n such that
;; GCD(i,n) = 1).


;;; Solution Part A
(define (filtered-accumulate passes-filter? combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (passes-filter? a)
                  (combiner result (term a))
                  result))))
  (iter a null-value))


(define (sum-prime-interval a b)
  (define (identity x) x)
  (define (increment x) (+ x 1))

  ;; prime? procedure from exercise 1.32
  (define (prime? n)
    (define (square x)
      (* x x))
    (define (divides? a b)
      (= (remainder b a) 0))
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))
    (define (smallest-divisor n)
      (find-divisor n 2))
    (= n (smallest-divisor n)))

  (filtered-accumulate prime? + 0 identity a increment b))

(sum-prime-interval 2 100) ; => 1060
(+ 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97) ; => 1060


;;; Solution Part B
(define (relative-prime-product n)
  (define (identity x) x)
  (define (increment x) (+ x 1))

  (define (is-relatively-prime-to-n? x)
    ;; gcd procedure from exercise 1.20
    (define (gcd i j)
      (if (= j 0)
          i
          (gcd j (remainder i j))))
    (= 1 (gcd x n)))

  (filtered-accumulate is-relatively-prime-to-n? * 1 identity 0 increment (- n 1)))

(relative-prime-product 10) ; => 189
(* 3 7 9) ; => 189
