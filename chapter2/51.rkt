#lang sicp
(#%require sicp-pict)

#| Exercise 2.51

Define the `below' operation for painters.  `Below' takes two painters
as arguments.  The resulting painter, given a frame, draws with the
first painter in the bottom of the frame and with the second painter
in the top.  Define `below' in two different ways--first by writing a
procedure that is analogous to the `beside' procedure given above, and
again in terms of `beside' and suitable rotation operations (from
Exercise 2-50). |#

;;; Solution
(paint einstein)

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)   ; new `origin'
                     (make-vect 0.0 0.0)   ; new end of `edge1'
                     (make-vect 1.0 1.0))) ; new end of `edge2'

(define (rotate-ccw-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)   ; new `origin'
                     (make-vect 0.0 1.0)   ; new end of `edge1'
                     (make-vect 1.0 0.0))) ; new end of `edge2'

(define (rotate-ccw-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new `origin'
                     (make-vect 0.0 0.0)   ; new end of `edge1'
                     (make-vect 1.0 1.0))) ; new end of `edge2'

;; `beside' definition from book
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


;; First `below' definition adapted from `beside'
(define (below bottom-painter top-painter)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-down
           (transform-painter bottom-painter
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-up
           (transform-painter top-painter
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-down frame)
        (paint-up frame)))))

(paint (below (flip-horiz einstein) einstein))

;; Second `below' definition using rotation transformations
(define (below-2 bottom-painter top-painter)
  (rotate-ccw-180 (rotate-ccw-270 (beside (rotate-ccw-270 bottom-painter)
                                          (rotate-ccw-270 top-painter)))))

(paint (below-2 (flip-horiz einstein) einstein))
