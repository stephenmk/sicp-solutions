#| Exercise 2.40

Define a procedure `unique-pairs' that, given an integer n, generates
the sequence of pairs (i,j) with 1 <= j< i <= n.  Use `unique-pairs'
to simplify the definition of `prime-sum-pairs' given above. |#

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

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

(unique-pairs 5)
;;Value: ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))

;; From exercise 1.22
(define (prime? n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (= n (smallest-divisor n)))


(define (prime-sum-pairs n)
  (define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))
  (define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))


(prime-sum-pairs 10)
;;Value: ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11) (7 4 11) (7 6 13) (8 3 11) (8 5 13) (9 2 11) (9 4 13) (9 8 17) (10 1 11) (10 3 13) (10 7 17) (10 9 19))
