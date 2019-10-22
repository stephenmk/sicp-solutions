#| Exercise 2.42

The "eight-queens puzzle" asks how to place eight queens on a
chessboard so that no queen is in check from any other
(i.e., no two queens are in the same row, column, or diagonal).  One
possible solution is shown in *Note Figure 2-8.  One way to solve the
puzzle is to work across the board, placing a queen in each column.
Once we have placed k - 1 queens, we must place the kth queen in a
position where it does not check any of the queens already on the
board.  We can formulate this approach recursively: Assume that we
have already generated the sequence of all possible ways to place k -
1 queens in the first k - 1 columns of the board.  For each of these
ways, generate an extended set of positions by placing a queen in each
row of the kth column.  Now filter these, keeping only the positions
for which the queen in the kth column is safe with respect to the
other queens.  This produces the sequence of all ways to place k
queens in the first k columns.  By continuing this process, we will
produce not only one solution, but all solutions to the puzzle.

We implement this solution as a procedure `queens', which returns a
sequence of all solutions to the problem of placing n queens on an
n*n chessboard.  `Queens' has an internal procedure `queen-cols'
that returns the sequence of all ways to place queens in the first
k columns of the board. |#

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

#| In this procedure `rest-of-queens' is a way to place k - 1 queens
in the first k - 1 columns, and `new-row' is a proposed row in which
to place the queen for the kth column.  Complete the program by
implementing the representation for sets of board positions, including
the procedure `adjoin-position', which adjoins a new row-column
position to a set of positions, and `empty-board', which represents an
empty set of positions.  You must also write the procedure `safe?',
which determines for a set of positions, whether the queen in the kth
column is safe with respect to the others.  (Note that we need only
check whether the new queen is safe--the other queens are already
guaranteed safe with respect to each other.) |#

;;; Solution
(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))


(define empty-board '())

(define (make-position column row)
  (list column row))

(define (get-row position)
  (cadr position))

(define (get-column position)
  (car position))

(define (get-position-by-column column positions)
  (car (filter (lambda (position) (= column (get-column position)))
               positions)))

(define (get-position-by-row row positions)
  (car (filter (lambda (position) (= row (get-row position)))
               positions)))

(define (adjoin-position new-row k rest-of-queens)
  (cons (make-position k new-row) rest-of-queens))

(define (safe? k positions)
  (let ((new-position (get-position-by-column k positions)))
    (fold-right
     (lambda (x y) (and x y))
     #t
     (map (lambda (position)
            (cond ((null? position) #t)
                  ((= (get-column new-position) (get-column position)) #t)
                  ((= (get-row new-position) (get-row position)) #f)
                  ((= (abs (- (get-column new-position) (get-column position)))
                      (abs (- (get-row new-position) (get-row position)))) #f)
                  (else #t)))
          positions))))

(define (display-row board-size row queen-column)
  (if (= 0 board-size)
      ((lambda ()
         (display "|")
         (newline)
         (if (= 1 row) (newline))))
      ((lambda ()
         (if (= 1 queen-column) (display "|x") (display "| "))
         (display-row (- board-size 1) row (- queen-column 1))))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (display-queens-solutions board-size)
  (fresh-line)
  (map (lambda (positions)
         (map (lambda (row)
                (display-row board-size
                             row
                             (get-column (get-position-by-row row positions))))
              (reverse (enumerate-interval 1 board-size))))
       (queens board-size)))

;; 2 solutions to 4x4 board
;; (display-queens-solutions 4)

#|

| |x| | |
| | | |x|
|x| | | |
| | |x| |

| | |x| |
|x| | | |
| | | |x|
| |x| | |

|#

;; (length (queens 8)) ;Value: 92

;; 92 solutions to 8x8 board
;; (display-queens-solutions 8)

#|

| | |x| | | | | |
| | | | | |x| | |
| | | |x| | | | |
| |x| | | | | | |
| | | | | | | |x|
| | | | |x| | | |
| | | | | | |x| |
|x| | | | | | | |

| | |x| | | | | |
| | | | |x| | | |
| |x| | | | | | |
| | | | | | | |x|
| | | | | |x| | |
| | | |x| | | | |
| | | | | | |x| |
|x| | | | | | | |

| | | | |x| | | |
| |x| | | | | | |
| | | |x| | | | |
| | | | | | |x| |
| | |x| | | | | |
| | | | | | | |x|
| | | | | |x| | |
|x| | | | | | | |

| | | |x| | | | |
| |x| | | | | | |
| | | | | | |x| |
| | |x| | | | | |
| | | | | |x| | |
| | | | | | | |x|
| | | | |x| | | |
|x| | | | | | | |

| | | |x| | | | |
| | | | | | |x| |
| | |x| | | | | |
| | | | | | | |x|
| |x| | | | | | |
| | | | |x| | | |
|x| | | | | | | |
| | | | | |x| | |

| | | | | |x| | |
| | |x| | | | | |
| | | | | | |x| |
| |x| | | | | | |
| | | | | | | |x|
| | | | |x| | | |
|x| | | | | | | |
| | | |x| | | | |

| | | | | |x| | |
| | |x| | | | | |
| | | | | | |x| |
| |x| | | | | | |
| | | |x| | | | |
| | | | | | | |x|
|x| | | | | | | |
| | | | |x| | | |

| | | | | |x| | |
| | | |x| | | | |
| |x| | | | | | |
| | | | | | | |x|
| | | | |x| | | |
| | | | | | |x| |
|x| | | | | | | |
| | |x| | | | | |

| | |x| | | | | |
| | | | | | |x| |
| |x| | | | | | |
| | | | | | | |x|
| | | | | |x| | |
| | | |x| | | | |
|x| | | | | | | |
| | | | |x| | | |

| | | | |x| | | |
| |x| | | | | | |
| | | |x| | | | |
| | | | | |x| | |
| | | | | | | |x|
| | |x| | | | | |
|x| | | | | | | |
| | | | | | |x| |

| | | |x| | | | |
| |x| | | | | | |
| | | | | | |x| |
| | |x| | | | | |
| | | | | |x| | |
| | | | | | | |x|
|x| | | | | | | |
| | | | |x| | | |

| |x| | | | | | |
| | | | | | |x| |
| | |x| | | | | |
| | | | | |x| | |
| | | | | | | |x|
| | | | |x| | | |
|x| | | | | | | |
| | | |x| | | | |

| | | | |x| | | |
| | |x| | | | | |
| | | | | | | |x|
| | | |x| | | | |
| | | | | | |x| |
|x| | | | | | | |
| | | | | |x| | |
| |x| | | | | | |

| | | |x| | | | |
| | | | | |x| | |
| | | | | | | |x|
| |x| | | | | | |
| | | | | | |x| |
|x| | | | | | | |
| | |x| | | | | |
| | | | |x| | | |

| | | |x| | | | |
| | | | | | |x| |
| | | | |x| | | |
| |x| | | | | | |
| | | | | |x| | |
|x| | | | | | | |
| | |x| | | | | |
| | | | | | | |x|

| | | | | | |x| |
| | |x| | | | | |
| | | | | | | |x|
| |x| | | | | | |
| | | | |x| | | |
|x| | | | | | | |
| | | | | |x| | |
| | | |x| | | | |

| | |x| | | | | |
| | | | | |x| | |
| | | | | | | |x|
| |x| | | | | | |
| | | |x| | | | |
|x| | | | | | | |
| | | | | | |x| |
| | | | |x| | | |

| | | | |x| | | |
| | | | | | |x| |
| |x| | | | | | |
| | | |x| | | | |
| | | | | | | |x|
|x| | | | | | | |
| | |x| | | | | |
| | | | | |x| | |

| | | | | | |x| |
| | | |x| | | | |
| |x| | | | | | |
| | | | | | | |x|
| | | | | |x| | |
|x| | | | | | | |
| | |x| | | | | |
| | | | |x| | | |

| | | | | | |x| |
| | | |x| | | | |
| |x| | | | | | |
| | | | |x| | | |
| | | | | | | |x|
|x| | | | | | | |
| | |x| | | | | |
| | | | | |x| | |

| | | | |x| | | |
| | | | | | |x| |
| |x| | | | | | |
| | | | | |x| | |
| | |x| | | | | |
|x| | | | | | | |
| | | | | | | |x|
| | | |x| | | | |

| | | | |x| | | |
| | | | | | |x| |
| |x| | | | | | |
| | | | | |x| | |
| | |x| | | | | |
|x| | | | | | | |
| | | |x| | | | |
| | | | | | | |x|

| | |x| | | | | |
| | | | | |x| | |
| |x| | | | | | |
| | | | | | |x| |
| | | | |x| | | |
|x| | | | | | | |
| | | | | | | |x|
| | | |x| | | | |

| | |x| | | | | |
| | | | | |x| | |
| |x| | | | | | |
| | | | |x| | | |
| | | | | | | |x|
|x| | | | | | | |
| | | | | | |x| |
| | | |x| | | | |

| | |x| | | | | |
| | | | | | |x| |
| |x| | | | | | |
| | | | | | | |x|
| | | | |x| | | |
|x| | | | | | | |
| | | |x| | | | |
| | | | | |x| | |

| | | |x| | | | |
| |x| | | | | | |
| | | | | | | |x|
| | | | |x| | | |
| | | | | | |x| |
|x| | | | | | | |
| | |x| | | | | |
| | | | | |x| | |

| | | |x| | | | |
| |x| | | | | | |
| | | | |x| | | |
| | | | | | | |x|
| | | | | |x| | |
|x| | | | | | | |
| | |x| | | | | |
| | | | | | |x| |

| |x| | | | | | |
| | | |x| | | | |
| | | | | |x| | |
| | | | | | | |x|
| | |x| | | | | |
|x| | | | | | | |
| | | | | | |x| |
| | | | |x| | | |

| | | |x| | | | |
| | | | | |x| | |
| | | | | | | |x|
| | |x| | | | | |
|x| | | | | | | |
| | | | | | |x| |
| | | | |x| | | |
| |x| | | | | | |

| | | |x| | | | |
| | | | | | |x| |
| | | | |x| | | |
| | |x| | | | | |
|x| | | | | | | |
| | | | | |x| | |
| | | | | | | |x|
| |x| | | | | | |

| | | |x| | | | |
| | | | | | | |x|
| | | | |x| | | |
| | |x| | | | | |
|x| | | | | | | |
| | | | | | |x| |
| |x| | | | | | |
| | | | | |x| | |

| | | | | |x| | |
| | |x| | | | | |
| | | | |x| | | |
| | | | | | | |x|
|x| | | | | | | |
| | | |x| | | | |
| |x| | | | | | |
| | | | | | |x| |

| | | | | |x| | |
| | |x| | | | | |
| | | | |x| | | |
| | | | | | |x| |
|x| | | | | | | |
| | | |x| | | | |
| |x| | | | | | |
| | | | | | | |x|

| | | | | |x| | |
| | |x| | | | | |
| | | | | | |x| |
| | | |x| | | | |
|x| | | | | | | |
| | | | | | | |x|
| |x| | | | | | |
| | | | |x| | | |

| | |x| | | | | |
| | | | |x| | | |
| | | | | | | |x|
| | | |x| | | | |
|x| | | | | | | |
| | | | | | |x| |
| |x| | | | | | |
| | | | | |x| | |

| | |x| | | | | |
| | | | | | | |x|
| | | |x| | | | |
| | | | | | |x| |
|x| | | | | | | |
| | | | | |x| | |
| |x| | | | | | |
| | | | |x| | | |

| | | | | |x| | |
| | | | | | | |x|
| |x| | | | | | |
| | | |x| | | | |
|x| | | | | | | |
| | | | | | |x| |
| | | | |x| | | |
| | |x| | | | | |

| | |x| | | | | |
| | | | |x| | | |
| |x| | | | | | |
| | | | | | | |x|
|x| | | | | | | |
| | | | | | |x| |
| | | |x| | | | |
| | | | | |x| | |

| | |x| | | | | |
| | | | | |x| | |
| |x| | | | | | |
| | | | | | |x| |
|x| | | | | | | |
| | | |x| | | | |
| | | | | | | |x|
| | | | |x| | | |

| | | |x| | | | |
| |x| | | | | | |
| | | | | | |x| |
| | | | |x| | | |
|x| | | | | | | |
| | | | | | | |x|
| | | | | |x| | |
| | |x| | | | | |

| | | |x| | | | |
| |x| | | | | | |
| | | | | | | |x|
| | | | | |x| | |
|x| | | | | | | |
| | |x| | | | | |
| | | | |x| | | |
| | | | | | |x| |

| | | | | | | |x|
| |x| | | | | | |
| | | | |x| | | |
| | |x| | | | | |
|x| | | | | | | |
| | | | | | |x| |
| | | |x| | | | |
| | | | | |x| | |

| | | | | | |x| |
| |x| | | | | | |
| | | | | |x| | |
| | |x| | | | | |
|x| | | | | | | |
| | | |x| | | | |
| | | | | | | |x|
| | | | |x| | | |

| |x| | | | | | |
| | | | | | |x| |
| | | | |x| | | |
| | | | | | | |x|
|x| | | | | | | |
| | | |x| | | | |
| | | | | |x| | |
| | |x| | | | | |

| |x| | | | | | |
| | | | |x| | | |
| | | | | | |x| |
| | | |x| | | | |
|x| | | | | | | |
| | | | | | | |x|
| | | | | |x| | |
| | |x| | | | | |

| |x| | | | | | |
| | | | | |x| | |
| | | | | | | |x|
| | |x| | | | | |
|x| | | | | | | |
| | | |x| | | | |
| | | | | | |x| |
| | | | |x| | | |

| | | | |x| | | |
| | | | | | |x| |
| | | |x| | | | |
|x| | | | | | | |
| | |x| | | | | |
| | | | | | | |x|
| | | | | |x| | |
| |x| | | | | | |

| | |x| | | | | |
| | | | | |x| | |
| | | | | | | |x|
|x| | | | | | | |
| | | |x| | | | |
| | | | | | |x| |
| | | | |x| | | |
| |x| | | | | | |

| | |x| | | | | |
| | | | | |x| | |
| | | |x| | | | |
|x| | | | | | | |
| | | | | | | |x|
| | | | |x| | | |
| | | | | | |x| |
| |x| | | | | | |

| | | | |x| | | |
| | | | | | | |x|
| | | |x| | | | |
|x| | | | | | | |
| | |x| | | | | |
| | | | | |x| | |
| |x| | | | | | |
| | | | | | |x| |

| | | | | |x| | |
| | | |x| | | | |
| | | | | | |x| |
|x| | | | | | | |
| | |x| | | | | |
| | | | |x| | | |
| |x| | | | | | |
| | | | | | | |x|

| | | | | | |x| |
| | | | |x| | | |
| | |x| | | | | |
|x| | | | | | | |
| | | | | |x| | |
| | | | | | | |x|
| |x| | | | | | |
| | | |x| | | | |

| | |x| | | | | |
| | | | | |x| | |
| | | | | | | |x|
|x| | | | | | | |
| | | | |x| | | |
| | | | | | |x| |
| |x| | | | | | |
| | | |x| | | | |

| | | | |x| | | |
| | | | | | | |x|
| | | |x| | | | |
|x| | | | | | | |
| | | | | | |x| |
| |x| | | | | | |
| | | | | |x| | |
| | |x| | | | | |

| | | | | |x| | |
| | | |x| | | | |
| | | | | | |x| |
|x| | | | | | | |
| | | | | | | |x|
| |x| | | | | | |
| | | | |x| | | |
| | |x| | | | | |

| | |x| | | | | |
| | | | |x| | | |
| | | | | | |x| |
|x| | | | | | | |
| | | |x| | | | |
| |x| | | | | | |
| | | | | | | |x|
| | | | | |x| | |

| | | | |x| | | |
| |x| | | | | | |
| | | | | |x| | |
|x| | | | | | | |
| | | | | | |x| |
| | | |x| | | | |
| | | | | | | |x|
| | |x| | | | | |

| | | | | |x| | |
| |x| | | | | | |
| | | | | | |x| |
|x| | | | | | | |
| | | |x| | | | |
| | | | | | | |x|
| | | | |x| | | |
| | |x| | | | | |

| | | | |x| | | |
| |x| | | | | | |
| | | | | | | |x|
|x| | | | | | | |
| | | |x| | | | |
| | | | | | |x| |
| | |x| | | | | |
| | | | | |x| | |

| | | | | | | |x|
| |x| | | | | | |
| | | |x| | | | |
|x| | | | | | | |
| | | | | | |x| |
| | | | |x| | | |
| | |x| | | | | |
| | | | | |x| | |

| | | | | | |x| |
| |x| | | | | | |
| | | |x| | | | |
|x| | | | | | | |
| | | | | | | |x|
| | | | |x| | | |
| | |x| | | | | |
| | | | | |x| | |

| | | | | |x| | |
| |x| | | | | | |
| | | | | | |x| |
|x| | | | | | | |
| | |x| | | | | |
| | | | |x| | | |
| | | | | | | |x|
| | | |x| | | | |

| |x| | | | | | |
| | | | | | | |x|
| | | | | |x| | |
|x| | | | | | | |
| | |x| | | | | |
| | | | |x| | | |
| | | | | | |x| |
| | | |x| | | | |

| |x| | | | | | |
| | | | |x| | | |
| | | | | | |x| |
|x| | | | | | | |
| | |x| | | | | |
| | | | | | | |x|
| | | | | |x| | |
| | | |x| | | | |

| | | | |x| | | |
| | | | | | |x| |
|x| | | | | | | |
| | |x| | | | | |
| | | | | | | |x|
| | | | | |x| | |
| | | |x| | | | |
| |x| | | | | | |

| | | | | | |x| |
| | |x| | | | | |
|x| | | | | | | |
| | | | | |x| | |
| | | | | | | |x|
| | | | |x| | | |
| |x| | | | | | |
| | | |x| | | | |

| | | | | |x| | |
| | |x| | | | | |
|x| | | | | | | |
| | | | | | |x| |
| | | | |x| | | |
| | | | | | | |x|
| |x| | | | | | |
| | | |x| | | | |

| | | | | |x| | |
| | | |x| | | | |
|x| | | | | | | |
| | | | |x| | | |
| | | | | | | |x|
| |x| | | | | | |
| | | | | | |x| |
| | |x| | | | | |

| | | |x| | | | |
| | | | | | |x| |
|x| | | | | | | |
| | | | | | | |x|
| | | | |x| | | |
| |x| | | | | | |
| | | | | |x| | |
| | |x| | | | | |

| | | |x| | | | |
| | | | | | | |x|
|x| | | | | | | |
| | | | |x| | | |
| | | | | | |x| |
| |x| | | | | | |
| | | | | |x| | |
| | |x| | | | | |

| | | | | | | |x|
| | | |x| | | | |
|x| | | | | | | |
| | |x| | | | | |
| | | | | |x| | |
| |x| | | | | | |
| | | | | | |x| |
| | | | |x| | | |

| | | |x| | | | |
| | | | | | | |x|
|x| | | | | | | |
| | |x| | | | | |
| | | | | |x| | |
| |x| | | | | | |
| | | | | | |x| |
| | | | |x| | | |

| | | | | |x| | |
| | |x| | | | | |
|x| | | | | | | |
| | | | | | | |x|
| | | | |x| | | |
| |x| | | | | | |
| | | |x| | | | |
| | | | | | |x| |

| | | | |x| | | |
| | |x| | | | | |
|x| | | | | | | |
| | | | | |x| | |
| | | | | | | |x|
| |x| | | | | | |
| | | |x| | | | |
| | | | | | |x| |

| | | | | |x| | |
| | |x| | | | | |
|x| | | | | | | |
| | | | | | | |x|
| | | |x| | | | |
| |x| | | | | | |
| | | | | | |x| |
| | | | |x| | | |

| | | | |x| | | |
| | | | | | |x| |
|x| | | | | | | |
| | | |x| | | | |
| |x| | | | | | |
| | | | | | | |x|
| | | | | |x| | |
| | |x| | | | | |

| | | |x| | | | |
| | | | | |x| | |
|x| | | | | | | |
| | | | |x| | | |
| |x| | | | | | |
| | | | | | | |x|
| | |x| | | | | |
| | | | | | |x| |

| | | | | | | |x|
| | |x| | | | | |
|x| | | | | | | |
| | | | | |x| | |
| |x| | | | | | |
| | | | |x| | | |
| | | | | | |x| |
| | | |x| | | | |

| | | | |x| | | |
| | |x| | | | | |
|x| | | | | | | |
| | | | | | |x| |
| |x| | | | | | |
| | | | | | | |x|
| | | | | |x| | |
| | | |x| | | | |

| |x| | | | | | |
| | | | | |x| | |
|x| | | | | | | |
| | | | | | |x| |
| | | |x| | | | |
| | | | | | | |x|
| | |x| | | | | |
| | | | |x| | | |

| | | |x| | | | |
|x| | | | | | | |
| | | | |x| | | |
| | | | | | | |x|
| | | | | |x| | |
| | |x| | | | | |
| | | | | | |x| |
| |x| | | | | | |

| | | | |x| | | |
|x| | | | | | | |
| | | | | | | |x|
| | | | | |x| | |
| | |x| | | | | |
| | | | | | |x| |
| |x| | | | | | |
| | | |x| | | | |

| | | | | | |x| |
|x| | | | | | | |
| | |x| | | | | |
| | | | | | | |x|
| | | | | |x| | |
| | | |x| | | | |
| |x| | | | | | |
| | | | |x| | | |

| | | | |x| | | |
|x| | | | | | | |
| | | |x| | | | |
| | | | | |x| | |
| | | | | | | |x|
| |x| | | | | | |
| | | | | | |x| |
| | |x| | | | | |

| | |x| | | | | |
|x| | | | | | | |
| | | | | | |x| |
| | | | |x| | | |
| | | | | | | |x|
| |x| | | | | | |
| | | |x| | | | |
| | | | | |x| | |

| | | | |x| | | |
|x| | | | | | | |
| | | | | | | |x|
| | | |x| | | | |
| |x| | | | | | |
| | | | | | |x| |
| | |x| | | | | |
| | | | | |x| | |

| | | |x| | | | |
|x| | | | | | | |
| | | | |x| | | |
| | | | | | | |x|
| |x| | | | | | |
| | | | | | |x| |
| | |x| | | | | |
| | | | | |x| | |

| | | | | |x| | |
|x| | | | | | | |
| | | | |x| | | |
| |x| | | | | | |
| | | | | | | |x|
| | |x| | | | | |
| | | | | | |x| |
| | | |x| | | | |

|x| | | | | | | |
| | | | |x| | | |
| | | | | | | |x|
| | | | | |x| | |
| | |x| | | | | |
| | | | | | |x| |
| |x| | | | | | |
| | | |x| | | | |

|x| | | | | | | |
| | | | | |x| | |
| | | | | | | |x|
| | |x| | | | | |
| | | | | | |x| |
| | | |x| | | | |
| |x| | | | | | |
| | | | |x| | | |

|x| | | | | | | |
| | | | | | |x| |
| | | |x| | | | |
| | | | | |x| | |
| | | | | | | |x|
| |x| | | | | | |
| | | | |x| | | |
| | |x| | | | | |

|x| | | | | | | |
| | | | | | |x| |
| | | | |x| | | |
| | | | | | | |x|
| |x| | | | | | |
| | | |x| | | | |
| | | | | |x| | |
| | |x| | | | | |

|#
