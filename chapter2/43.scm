#| Exercise 2.43

Louis Reasoner is having a terrible time doing Exercise 2-42.  His
`queens' procedure seems to work, but it runs extremely
slowly.  (Louis never does manage to wait long enough for it to solve
even the 6*6 case.)  When Louis asks Eva Lu Ator for help, she points
out that he has interchanged the order of the nested mappings in the
`flatmap', writing it as

          (flatmap
           (lambda (new-row)
             (map (lambda (rest-of-queens)
                    (adjoin-position new-row k rest-of-queens))
                  (queen-cols (- k 1))))
           (enumerate-interval 1 board-size))

Explain why this interchange makes the program run slowly.  Estimate
how long it will take Louis's program to solve the eight-queens
puzzle, assuming that the program in *Note Exercise 2-42 solves the
puzzle in time T. |#


;;; Solution

;; The recursive `queen-cols' procedure is called more often in this
;; order, resulting in redundant calculations.

(load "42.scm")

;; Original order
(define (queens board-size)
  (fresh-line)
  (define (queen-cols k)
    (display k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 4)
;; 43210
;; `queen-cols' is executed once per k

;; Louis Reasoner's order
(define (queens board-size)
  (fresh-line)
  (define (queen-cols k)
    (display k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
          (flatmap
           (lambda (new-row)
             (map (lambda (rest-of-queens)
                    (adjoin-position new-row k rest-of-queens))
                  (queen-cols (- k 1))))
           (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(queens 4)

;; 43210000100001000010000
;;   210000100001000010000
;;   210000100001000010000
;;   210000100001000010000
;;  3210000100001000010000
;;   210000100001000010000
;;   210000100001000010000
;;   210000100001000010000
;;  3210000100001000010000
;;   210000100001000010000
;;   210000100001000010000
;;   210000100001000010000
;;  3210000100001000010000
;;   210000100001000010000
;;   210000100001000010000
;;   210000100001000010000

;; Here, (queen-cols 0) is executed 256 times,
;;       (queen-cols 1) 64 times,
;;       (queen-cols 2) 16 times,
;;   and (queen-cols 3) 4 times.
