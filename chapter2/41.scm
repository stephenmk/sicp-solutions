#| Exercise 2.41

Write a procedure to find all ordered triples of distinct positive
integers i, j, and k less than or equal to a given integer n that sum
to a given integer s. |#

;;; Solution
;; Strategy is to enumerate all the ordered triplets, then test each
;; sum for equality with `s'.

(define accumulate fold-right)

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;; For each unique pair (i k), append j, i<j<k, to create sets of
;; unique triplets (i j k)
(define (unique-triplets n)
  (flatmap (lambda (pair)
             (let ((i (min (car pair) (cadr pair)))
                   (k (max (car pair) (cadr pair))))
               (map (lambda (j) (list i j k))
                    (enumerate-interval (+ i 1) (- k 1)))))
           (unique-pairs n)))

(unique-triplets 5)
;;Value: ((1 2 3) (1 2 4) (1 3 4) (2 3 4) (1 2 5) (1 3 5) (1 4 5) (2 3 5) (2 4 5) (3 4 5))

(define (sum-triplets n s)
  (define (sum-triplet? triplet)
    (= s (+ (car triplet) (cadr triplet) (caddr triplet))))
  (define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
  (filter sum-triplet?
          (unique-triplets n)))


(sum-triplets 5 7)
;;Value: ((1 2 4))

(sum-triplets 5 10)
;;Value: ((1 4 5) (2 3 5))

(sum-triplets 5 13)
;;Value: ()
