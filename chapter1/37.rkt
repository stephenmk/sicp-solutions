(require sicp)


;;; Exercise 1.37.
;;; Part A.
;; An infinite continued fraction is an expression of the form
;;
;; (N_1) / (D_1 + (N_2 / (D_2 + N_3 / (D_3 + ...))))
;;
;; As an example, one can show that the infinite continued fraction
;; expansion with the N_i and the D_i all equal to 1 produces 1/φ,
;; where φ is the golden ratio (described in section 1.2.2). One way
;; to approximate an infinite continued fraction is to truncate the
;; expansion after a given number of terms. Such a truncation -- a
;; so-called k-term finite continued fraction -- has the form
;;
;; (N_1) / (D_1 + (N_2 / ... + (N_k / D_k) ))
;;
;; Suppose that n and d are procedures of one argument (the term index
;; i) that return the N_i and D_i of the terms of the continued
;; fraction. Define a procedure cont-frac such that evaluating
;; (cont-frac n d k) computes the value of the k-term finite continued
;; fraction. Check your procedure by approximating 1/φ using

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)

;; for successive values of k. How large must you make k in order to
;; get an approximation that is accurate to 4 decimal places?

;;; Part B.
;; If your cont-frac procedure generates a recursive process, write
;; one that generates an iterative process. If it generates an
;; iterative process, write one that generates a recursive process.


;;; Solution A
;; By definition, φ = (1 + √5) / 2
;; So rounded to 4 decimal places, 1/φ equals 0.6180

(define (cont-frac n d k)
  (define (cont-frac-recursive i)
    (/ (n i)
       (+ (d i)
          (if (= i k)
              0
              (cont-frac-recursive (+ i 1))))))
  (cont-frac-recursive 1))

(define (find-k k)
  (let ((estimate (cont-frac (lambda (i) 1.0)
                             (lambda (i) 1.0)
                             k))
        (tolerance 0.0001)
        (real-value 0.6180))
    (if (< (abs (- real-value estimate)) tolerance)
        k
        (find-k (+ k 1)))))

(find-k 1) ; => 10

;; k must be set to 10 to get an approximation that is accurate to 4
;; decimal places.


;;; Solution B
(define (cont-frac n d k)
  (define (cont-frac-iterative i v)
    (if (= i 0)
        v
        (cont-frac-iterative (- i 1)
                             (/ (n i) (+ (d i) v)))))
  (cont-frac-iterative k 0))

(find-k 1) ; => 10
;; Same result as recursive process
