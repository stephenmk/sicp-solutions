#| Exercise 2.9

The "width" of an interval is half of the difference between its upper
and lower bounds.  The width is a measure of the uncertainty of the
number specified by the interval.  For some arithmetic operations the
width of the result of combining two intervals is a function only of
the widths of the argument intervals, whereas for others the width of
the combination is not a function of the widths of the argument
intervals.  Show that the width of the sum (or difference) of two
intervals is a function only of the widths of the intervals being
added (or subtracted).  Give examples to show that this is not true
for multiplication or division. |#


#| Solution

Let the width of interval x = [x_L, x_U] be defined as

x_W = (x_U - x_L) / 2

Then for interval z = x + y,

z_W = ((x_U + y_U) - (x_L + y_L)) / 2
    = ((x_U - x_L)/2) + ((y_U - y_L)/2)
    = x_W + y_W

For interval subtraction, use the formula from the previous exercise.

z = x - y = [(x_L - y_U), (x_U - y_L)]

z_W = ((x_U - y_L) - (x_L - y_U)) / 2
    = ((x_U - x_L) + (y_U - y_L)) / 2
    = x_W + y_W

For both interval addition and subtraction, the result can be
expressed in terms of the interval widths. |#


;;; Multiplication and Division
(define (make-interval a b) (cons a b))
(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define a (make-interval -1.0 1.0))
(define b (make-interval  4.0 6.0))
(define x (make-interval -4.0 -2.0))
(define y (make-interval  1.0 3.0))

(mul-interval a b) ; (-6 . 6)
(mul-interval x y) ; (-12 . -2)

(div-interval a b) ; (-25 . 25)
(div-interval x y) ; (-4 . -0.66666)

;; Different results despite all intervals having a width of 1
