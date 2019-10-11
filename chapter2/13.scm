#| Exercise 2.13

Show that under the assumption of small percentage tolerances there is
a simple formula for the approximate percentage tolerance of the
product of two intervals in terms of the tolerances of the factors.
You may simplify the problem by assuming that all numbers are
positive. |#


#| Solution

For positive intervals (xL, xU) and (yL, yU), the multiplied interval
is (xL*yL, xU*yU). The width `W' of this interval is then

2W = xU*yU - xL*yL
   = xU*yU - xL*yL + (xL*yU - xL*yU)
   = yU * (xU - xL) + xL * (yU - yL)

and the center `C' of the interval is

2C = xU*yU + xL*yL

and so the tolerance `P' is

P = W / C

    yU * (xU - xL)     xL * (yU - yL)
=   --------------  +  --------------
    xU*yU + xL*yL      xU*yU + xL*yL


       (xU - xL)           (yU - yL)
=   ---------------  +  ---------------
    xU + xL*(yL/yU)     (xU/xL)*yU + yL


For small tolerances, the ratios (yL/yU) and (xU/xL) are both roughly
equal to one. The tolerance of the multiplied interval is therefore
approximately equal to the sum of the tolerances of `x' and `y.'

|#
