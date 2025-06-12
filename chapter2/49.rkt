#lang sicp
(#%require sicp-pict)

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
   (list (make-segment (make-vect 0 0) ; bottom side
                       (make-vect 1 0))
         (make-segment (make-vect 1 0) ; right side
                       (make-vect 1 1))
         (make-segment (make-vect 1 1) ; top side
                       (make-vect 0 1))
         (make-segment (make-vect 0 1) ; left side
                       (make-vect 0 0)))))

(paint outline-painter)

;; We can extract the procedure for producing the segment list in
;; order to make the definition more data driven.
(define (make-segment-list coord-list)
  (if (null? coord-list) '()
      (let ((coord (car coord-list)))
        (append (list (make-segment
                       (make-vect (caar coord)
                                  (cadar coord))
                       (make-vect (caadr coord)
                                  (cadadr coord))))
                (make-segment-list (cdr coord-list))))))

(define outline-painter2
  (segments->painter
   (make-segment-list '(((0 0) (1 0))     ; bottom side
                        ((1 0) (1 1))     ; right side
                        ((1 1) (0 1))     ; top side
                        ((0 1) (0 0)))))) ; left side

(paint outline-painter2)

(define x-painter
  (segments->painter
   (make-segment-list '(((0 0) (1 1))
                        ((1 0) (0 1))))))

(paint x-painter)

(define diamond-painter
  (segments->painter
   (make-segment-list '(((0.5 0) (0 0.5))     ; lower-left side
                        ((0.5 0) (1 0.5))     ; lower-right side
                        ((0 0.5) (0.5 1))     ; upper-left side
                        ((1 0.5) (0.5 1)))))) ; upper-right side

(paint diamond-painter)

;; The wave painter is composed of 17 segments.
(define wave-painter
  (segments->painter
   (make-segment-list '(((0.2 0.0) (0.4 0.6))      ; 1
                        ((0.4 0.6) (0.3 0.7))      ; 2
                        ((0.3 0.7) (0.1 0.4))      ; 3
                        ((0.1 0.4) (0.0 0.6))      ; 4
                        ((0.0 0.9) (0.1 0.7))      ; 5
                        ((0.1 0.7) (0.3 0.8))      ; 6
                        ((0.3 0.8) (0.4 0.8))      ; 7
                        ((0.4 0.8) (0.3 0.9))      ; 8
                        ((0.3 0.9) (0.4 1.0))      ; 9
                        ((0.5 1.0) (0.6 0.9))      ; 10
                        ((0.6 0.9) (0.5 0.8))      ; 11
                        ((0.5 0.8) (0.6 0.8))      ; 12
                        ((0.6 0.8) (1.0 0.5))      ; 13
                        ((1.0 0.3) (0.6 0.6))      ; 14
                        ((0.6 0.6) (0.8 0.0))      ; 15
                        ((0.6 0.0) (0.5 0.4))      ; 16
                        ((0.5 0.4) (0.4 0.0))))))  ; 17

(paint wave-painter)
