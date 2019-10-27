#| Exercise 2.46

A two-dimensional vector v running from the origin to a point can be
represented as a pair consisting of an x-coordinate and a
y-coordinate.  Implement a data abstraction for vectors by giving a
constructor `make-vect' and corresponding selectors `xcor-vect' and
`ycor-vect'.  In terms of your selectors and constructor, implement
procedures `add-vect', `sub-vect', and `scale-vect' that perform the
operations vector addition, vector subtraction, and multiplying a
vector by a scalar:

(x_1, y_1) + (x_2, y_2) = (x_1 + x_2, y_1 + y_2)

(x_1, y_1) - (x_2, y_2) = (x_1 - x_2, y_1 - y_2)

s * (x, y) = (sx, sy)

|#

;;; Solution

(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1) (xcor-vect vect2))
             (+ (ycor-vect vect1) (ycor-vect vect2))))

(define (sub-vect vect1 vect2)
  (make-vect (- (xcor-vect vect1) (xcor-vect vect2))
             (- (ycor-vect vect1) (ycor-vect vect2))))

(define (scale-vect vect s)
  (make-vect (* s (xcor-vect vect))
             (* s (ycor-vect vect))))
