;; Exercise 1.10

;; The following procedure computes a mathematical function called Ackermann's function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; What are the values of the following expressions?
;; Give concise mathematical definitions for Ackermann's function
;; in terms of y (for positive integer values of y) when x = 0, 1, and 2.
(A 1 10)
(A 2 4)
(A 3 3)

;; Solutions
(A 1 10)
(A (- 1 1) (A 1 (- 10 1)))
(A 0 (A 1 9))
(A 0 (A (- 1 1) (A 1 (- 9 1))))
(A 0 (A 0 (A 1 8))) ; Pattern becoming clear.
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024

;; At this point, we can see that when x = 1,
;; the y argument is multiplied by 2 a number of times equal to y.
;; Expressed as a closed-form function,
;; A(1,y) = 2^y

;; We've also made clear that when x is 0,
;; then the function merely returns y multiplied by 2.
;; A(0,y) = 2*y

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2))) ; 2^2 = 4
(A 1 (A 1 4)) ; 2^4 = 16 = 2^2^2
(A 1 16) ; 2^16 = 65536 = 2^2^2^2
65536

;; We see that setting x=2 results in the A1 process being called
;; on itself y number of times, starting from 1. In other words,
;; A(2,4) = 2^(2^(2^(2^1))) = 2^(2^(2^2)) = 2^(2^4) = 2^16 = 65536
;; Expressed in a general concise form, this is
;; A(2,y) = 2^2^2^2...^2 with y-1 exponents

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2)) ; 2^2 = 4
(A 2 4) ; 2^2^2^2 = 2^2^4 = 2^16 = 65536
65536
