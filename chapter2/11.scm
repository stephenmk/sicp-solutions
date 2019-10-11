#| Exercise 2.11

In passing, Ben also cryptically comments: "By testing the signs of
the endpoints of the intervals, it is possible to break `mul-interval'
into nine cases, only one of which requires more than two
multiplications."  Rewrite this procedure using Ben's suggestion. |#

#| Solution

The nine cases are displayed in the following table.

| x_l | x_u         | y_l | y_u         | interval                   |
|-----+-------------+-----+-------------+----------------------------|
| +   | + (implied) | +   | + (implied) | [(x_l * y_l), (x_u * y_u)] |
| -   | +           | +   | + (implied) | [(x_l * y_u), (x_u * y_u)] |
| -   | -           | +   | + (implied) | [(x_l * y_u), (x_u * y_l)] |
| +   | + (implied) | -   | +           | [(x_u * y_l), (x_u * y_u)] |
| -   | +           | -   | +           | Use old method             |
| -   | -           | -   | +           | [(x_l * y_u), (x_l * y_l)] |
| +   | + (implied) | -   | -           | [(x_u * y_l), (x_l * y_u)] |
| -   | +           | -   | -           | [(x_u * y_l), (x_l * y_l)] |
| -   | -           | -   | -           | [(x_u * y_u), (x_l * y_l)] |

|#

(define (make-interval a b) (cons a b))
(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))

(define (mul-interval x y)
  (let ((xL (lower-bound x))
        (xU (upper-bound x))
        (yL (lower-bound y))
        (yU (upper-bound y)))
    (cond ((and (> xL 0) (> xU 0) (> yL 0) (> yU 0))
           (make-interval (* xL yL) (* xU yU)))
          ((and (< xL 0) (> xU 0) (> yL 0) (> yU 0))
           (make-interval (* xL yU) (* xU yU)))
          ((and (< xL 0) (< xU 0) (> yL 0) (> yU 0))
           (make-interval (* xL yU) (* xU yL)))
          ((and (> xL 0) (> xU 0) (< yL 0) (> yU 0))
           (make-interval (* xU yL) (* xU yU)))
          ((and (< xL 0) (< xU 0) (< yL 0) (> yU 0))
           (make-interval (* xL yU) (* xL yL)))
          ((and (> xL 0) (> xU 0) (< yL 0) (< yU 0))
           (make-interval (* xU yL) (* xL yU)))
          ((and (< xL 0) (> xU 0) (< yL 0) (< yU 0))
           (make-interval (* xU yL) (* xL yL)))
          ((and (< xL 0) (< xU 0) (< yL 0) (< yU 0))
           (make-interval (* xU yU) (* xL yL)))
          (else (let ((p1 (* xL yL))
                      (p2 (* xL yU))
                      (p3 (* xU yL))
                      (p4 (* xU yU)))
                  (make-interval (min p1 p2 p3 p4)
                                 (max p1 p2 p3 p4)))))))

(define (mul-interval-original x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define a (make-interval 2 3))
(define b (make-interval -5 7))
(define c (make-interval -13 11))
(define d (make-interval -17 -19))

(define (test-mul-intervals x y)
  (newline)
  (display (mul-interval x y))
  (newline)
  (display (mul-interval-original x y))
  (newline))

(test-mul-intervals a a)
(test-mul-intervals a b)
(test-mul-intervals a c)
(test-mul-intervals a d)
(test-mul-intervals b b)
(test-mul-intervals b c)
(test-mul-intervals b d)
(test-mul-intervals c c)
(test-mul-intervals c d)
(test-mul-intervals d d)

#| Results

(4 . 9)
(4 . 9)

(-15 . 21)
(-15 . 21)

(-39 . 33)
(-39 . 33)

(-57 . -34)
(-57 . -34)

(-35 . 49)
(-35 . 49)

(-91 . 77)
(-91 . 77)

(-133 . 95)
(-133 . 95)

(-143 . 169)
(-143 . 169)

(-209 . 247)
(-209 . 247)

(289 . 361)
(289 . 361)

|#
