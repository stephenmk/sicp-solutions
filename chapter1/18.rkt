(require sicp)

;;; Exercise 1.18.
;; Using the results of exercises 1.16 and 1.17, devise a procedure
;; that generates an iterative process for multiplying two integers in
;; terms of adding, doubling, and halving and uses a logarithmic
;; number of steps.

;; This algorithm, which is sometimes known as the ``Russian peasant
;; method'' of multiplication, is ancient. Examples of its use are
;; found in the Rhind Papyrus, one of the two oldest mathematical
;; documents in existence, written about 1700 B.C. (and copied from an
;; even older document) by an Egyptian scribe named A'h-mose.


;;; Solution
;; I use the iterative state variable to store the deferred addition
;; operations that result from the application of the formula
;;  ab = ((a-1)+1)+b = ((a-1)b) + b
;; The state variable is then added to the final product at the
;; end of the process.

(define (fast-multiply a b)
  (define (double x)
    (* x 2))

  (define (halve x)
    (/ x 2))

  (define (even? x)
    (= (remainder x 2) 0))

  (define (abs x)
    (if (< x 0) (* x -1) x))

  (define (fast-multiply-iter i j k)
    (cond ((= i 1) (+ j k))
          ((even? i) (fast-multiply-iter (halve i) (double j) k))
          (else (fast-multiply-iter (- i 1) j (+ j k))
          )))

  ;; Check the arguments before running the process
  (cond ((= a 0) 0)
        ((= b 0) 0)
        ((= a 1) b)
        ((= b 1) a)
        ((and (< a 0) (< b 0)) (fast-multiply (abs a) (abs b)))
        ((< a 0) (fast-multiply (abs a) (* b -1)))
        (else (fast-multiply-iter a b 0))))

(fast-multiply 5 21) ; => 105
(fast-multiply 44 15) ; => 660
