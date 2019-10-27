#| Exercise 2.49

Use `segments->painter' to define the following primitive painters:

a. The painter that draws the outline of the designated frame.

b. The painter that draws an "X" by connecting opposite corners
of the frame.

c. The painter that draws a diamond shape by connecting the
midpoints of the sides of the frame.

d. The `wave' painter. |#

;;; Solution

(define outline-painter
  (segments->painter
   (list (make-segment (make-vector 0 0) ; bottom side
                       (make-vector 1 0))
         (make-segment (make-vector 1 0) ; right side
                       (make-vector 1 1))
         (make-segment (make-vector 1 1) ; top side
                       (make-vector 0 1))
         (make-segment (make-vector 0 1) ; left side
                       (make-vector 0 0)))))

(define x-painter
  (segments->painter
   (list (make-segment (make-vector 0 0) ; lower-left to upper-right
                       (make-vector 1 1))
         (make-segment (make-vector 1 0) ; lower-right to upper-left
                       (make-vector 0 1)))))

(define diamond-painter
  (segments->painter
   (list (make-segment (make-vector .5  0) ; lower-left side
                       (make-vector  0 .5))
         (make-segment (make-vector .5  0) ; lower-right side
                       (make-vector  1 .5))
         (make-segment (make-vector  0 .5) ; upper-left side
                       (make-vector .5  1))
         (make-segment (make-vector  1 .5) ; upper-right side
                       (make-vector .5  1)))))

;; TODO: wave painter
