(require sicp)

;;; Exercise 1.21
;; Use the smallest-divisor procedure to find the smallest divisor of
;; each of the following numbers: 199, 1999, 19999.

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

;;; Solution
;;
;; |-----+--------------+-----------------------+----------------------------|
;; |   n | test-divisor | (square test-divisor) | (remainder n test-divisor) |
;; |-----+--------------+-----------------------+----------------------------|
;; | 199 |            2 |                     4 |                          3 |
;; | 199 |            3 |                     9 |                          1 |
;; | 199 |            4 |                    16 |                          3 |
;; | 199 |            5 |                    25 |                          4 |
;; | 199 |            6 |                    36 |                          1 |
;; | 199 |            7 |                    49 |                          3 |
;; | 199 |            8 |                    64 |                          7 |
;; | 199 |            9 |                    81 |                          1 |
;; | 199 |           10 |                   100 |                          9 |
;; | 199 |           11 |                   121 |                          1 |
;; | 199 |           12 |                   144 |                          7 |
;; | 199 |           13 |                   169 |                          4 |
;; | 199 |           14 |                   196 |                          3 |
;; | 199 |           15 |                   225 |                            |
;; |-----+--------------+-----------------------+----------------------------|
;;
;; 15 squared is larger than 199, so 199 is prime.
;;
;; For n=1999, the process continues similarly. Up to test-divisor=44,
;; none of the remainders are equal to zero. 45 squared is larger than
;; 1999, so 1999 is prime.
;;
;; |-------+--------------+-----------------------+----------------------------|
;; |     n | test-divisor | (square test-divisor) | (remainder n test-divisor) |
;; |-------+--------------+-----------------------+----------------------------|
;; | 19999 |            2 |                     4 |                          1 |
;; | 19999 |            3 |                     9 |                          1 |
;; | 19999 |            4 |                    16 |                          3 |
;; | 19999 |            5 |                    25 |                          4 |
;; | 19999 |            6 |                    36 |                          1 |
;; | 19999 |            7 |                    49 |                          0 |
;; |-------+--------------+-----------------------+----------------------------|
;;
;; 19999 % 7 = 0, so 7 is the smallest divisor of 19999.
