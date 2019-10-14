#| Exercise 2.37

Suppose we represent vectors v = (v_i) as sequences of numbers, and
matrices m = (m_(ij)) as sequences of vectors (the rows of the
matrix).  For example, the matrix

     +-         -+
     |  1 2 3 4  |
     |  4 5 6 6  |
     |  6 7 8 9  |
     +-         -+

is represented as the sequence `((1 2 3 4) (4 5 6 6) (6 7 8 9))'.
With this representation, we can use sequence operations to concisely
express the basic matrix and vector operations.  These operations
(which are described in any book on matrix algebra) are the following:

     (dot-product v w)      returns the sum Σ_i v_i w_i

     (matrix-*-vector m v)  returns the vector t,
                            where t_i = Σ_j m_(ij) v_j

     (matrix-*-matrix m n)  returns the matrix p,
                            where p_(ij) = Σ_k m_(ik) n_(kj)

     (transpose m)          returns the matrix n,
                            where n_(ij) = m_(ji)

   We can define the dot product as

     (define (dot-product v w)
       (accumulate + 0 (map * v w)))

   Fill in the missing expressions in the following procedures for
computing the other matrix operations.  (The procedure `accumulate-n'
is defined in *Note Exercise 2-36)

     (define (matrix-*-vector m v)
       (map <??> m))

     (define (transpose mat)
       (accumulate-n <??> <??> mat))

     (define (matrix-*-matrix m n)
       (let ((cols (transpose n)))
         (map <??> m)))

|#

;;; Solution

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; matrix-*-vector: dot product of each row of the matrix `m' with the vector
;; `v', returned in a list
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(define v1 '(1 2 3))
(define v2 '(4 5 6))
(define v3 '(7 8 9))
(define m1 (list v1 v2 v3))

(dot-product v1 v1) ;Value: 14
(dot-product v1 v2) ;Value: 32
(dot-product v1 v3) ;Value: 50
(matrix-*-vector m1 v1) ;Value: (14 32 50)


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; the first elements of each row become the new first row, and so on.
(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose m1) ;Value: ((1 4 7) (2 5 8) (3 6 9))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (w) (matrix-*-vector cols w)) m)))

#|
   |1 2 3|   |1 2 3|
   |4 5 6| * |4 5 6|
   |7 8 9|   |7 8 9|

   |(1*1 + 2*4 + 3*7) (1*2 + 2*5 + 3*8) (1*3 + 2*6 + 3*9)|
 = |(4*1 + 5*4 + 6*7) (4*2 + 5*5 + 6*8) (4*3 + 5*6 + 6*9)|
   |(7*1 + 8*4 + 9*7) (7*2 + 8*5 + 9*8) (7*3 + 8*6 + 9*9)|

   | 30  36  42|
 = | 66  81  96|
   |102 126 150|
|#

(matrix-*-matrix m1 m1) ;Value: ((30 36 42) (66 81 96) (102 126 150))
