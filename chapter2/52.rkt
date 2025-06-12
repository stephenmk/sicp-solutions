#lang sicp
(#%require sicp-pict)

#| Exercise 2.52

Make changes to the square limit of `wave' shown in Figure 2.9 by
working at each of the levels described above. In particular:

       a. Add some segments to the primitive `wave' painter of
          exercise 2.49 (to add a smile, for example).

       b. Change the pattern constructed by `corner-split' (for
          example, by using only one copy of the `up-split' and
          `right-split' images instead of two).

       c. Modify the version of `square-limit' that uses
          `square-of-four' so as to assemble the corners in a
          different pattern. (For example, you might make the big
          Mr. Rogers look outward from each corner of the square.)

|#

;;; Solution

;; Copying definitions of `right-split' and `upsplit' from Exercise 2.44
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; Definition of `corner-split' from section 2.4.4
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


;;; Solution (a)

;; Copying `make-segment-list` from solution to Exercise 2.49.
(define (make-segment-list coord-list)
  (if (null? coord-list) '()
      (let ((coord (car coord-list)))
        (append (list (make-segment
                       (make-vect (caar coord)
                                  (cadar coord))
                       (make-vect (caadr coord)
                                  (cadadr coord))))
                (make-segment-list (cdr coord-list))))))

;; Adding 3 segments to make a smile
(define wave-painter
  (segments->painter
   (make-segment-list '(((0.375 0.9) (0.425 0.85))
                        ((0.425 0.85) (0.475 0.85))
                        ((0.475 0.85) (0.525 0.9))
                        ((0.2 0.0) (0.4 0.6))
                        ((0.4 0.6) (0.3 0.7))
                        ((0.3 0.7) (0.1 0.4))
                        ((0.1 0.4) (0.0 0.6))
                        ((0.0 0.9) (0.1 0.7))
                        ((0.1 0.7) (0.3 0.8))
                        ((0.3 0.8) (0.4 0.8))
                        ((0.4 0.8) (0.3 0.9))
                        ((0.3 0.9) (0.4 1.0))
                        ((0.5 1.0) (0.6 0.9))
                        ((0.6 0.9) (0.5 0.8))
                        ((0.5 0.8) (0.6 0.8))
                        ((0.6 0.8) (1.0 0.5))
                        ((1.0 0.3) (0.6 0.6))
                        ((0.6 0.6) (0.8 0.0))
                        ((0.6 0.0) (0.5 0.4))
                        ((0.5 0.4) (0.4 0.0))))))

(display "Wave painter from exercise 2.49 with an added smile")
(newline)
(paint wave-painter)


;;; Solution (b)

(define (modified-corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (modified-corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(display "Original corner-split pattern")
(newline)
(paint (corner-split wave-painter 4))
(display "New corner-split pattern with one copy in each direction instead of two")
(newline)
(paint (modified-corner-split wave-painter 4))

;;; Solution (c)

;; `square-of-four' and `square-limit' procedures from section 2.2.4
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(display "Original square-limit einstein (facing outward)")
(newline)
(paint (square-limit einstein 4))


(define (modified-square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split (flip-horiz painter) n))))

(display "New square-limit einstein (facing inward)")
(newline)
(paint (modified-square-limit einstein 4))
