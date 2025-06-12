#lang sicp
(#%require sicp-pict)

#| Exercise 2.50

Define the transformation `flip-horiz', which flips painters
horizontally, and transformations that rotate painters
counterclockwise by 180 degrees and 270 degrees. |#

;;; Solution
(display "Original")
(newline)
(paint einstein)

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)   ; new `origin'
                     (make-vect 0.0 0.0)   ; new end of `edge1'
                     (make-vect 1.0 1.0))) ; new end of `edge2'

(display "Horizontally flipped")
(newline)
(paint (flip-horiz einstein))

(define (rotate-ccw-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)   ; new `origin'
                     (make-vect 0.0 1.0)   ; new end of `edge1'
                     (make-vect 1.0 0.0))) ; new end of `edge2'

(display "Original rotated 180 degrees counter-clockwise")
(newline)
(paint (rotate-ccw-180 einstein))

(define (rotate-ccw-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new `origin'
                     (make-vect 0.0 0.0)   ; new end of `edge1'
                     (make-vect 1.0 1.0))) ; new end of `edge2'

(display "Original rotated 270 degrees counter-clockwise")
(newline)
(paint (rotate-ccw-270 einstein))

(display "Four 270 rotations = Three 360 rotations = back to original")
(newline)
(paint (rotate-ccw-270
        (rotate-ccw-270
         (rotate-ccw-270
          (rotate-ccw-270 einstein)))))
