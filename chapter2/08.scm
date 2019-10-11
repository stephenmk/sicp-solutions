#| Exercise 2.8

Using reasoning analogous to Alyssa's, describe how the difference of
two intervals may be computed.  Define a corresponding subtraction
procedure, called `sub-interval'.|#

#| Solution

Let x_0 be a number on interval (x_L, x_U) such that x_L < x_0 < x_U.
Let y_0 be a number on interval (y_L, y_U) such that y_L < y_0 < y_U.

Then x_0 + y_L < x_U + y_0
  => x_0 - y_0 < x_U - y_L

and x_L + y_0 < x_0 + y_U
 => x_L - y_U < x_0 - y_0

Combined, this gives us

x_L - y_U < x_0 - y_0 < x_U - y_L

meaning that the interval subtraction extrema will always
be (x_L - y_U) and (x_U - y_L).

|#

(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))))
