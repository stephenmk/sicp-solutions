(require sicp)

;; Exercise 1.12
;; The following pattern of numbers is called Pascal's triangle.
;;     1
;;    1 1
;;   1 2 1
;;  1 3 3 1
;; 1 4 6 4 1
;;    ...
;; The numbers at the edge of the triangle are all 1, and each number
;; inside the triangle is the sum of the two numbers above it. Write a
;; procedure that computes elements of Pascal's triangle by means of a
;; recursive process.


;; Solution

;; Let n be the row number (starting from n=0) and k be the index of
;; the desired element within the row, 0 <= k <= n.

;; Let B(n,k) be the function which computes the elements of the
;; triangle (i.e., the binomial coefficient).

;; Then B(n,0) = B(n,n) = 1, because the edges are always equal to 1.
;; And B(n,k) = B(n-1,k-1) + B(n-1,k), because each number is the sum
;; of the two numbers above it.

(define (B n k)
  (cond ((= n 0) 1)
        ((= n k) 1)
        ((= 0 k) 1)
        (else (+ (B (- n 1) (- k 1))
                 (B (- n 1) k)))))

;; Example
;: (B 8 4) ; => 70
