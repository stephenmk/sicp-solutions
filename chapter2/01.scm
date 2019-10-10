#| Exercise 2.1.

Define a better version of make-rat that handles both positive and
negative arguments. Make-rat should normalize the sign so that if the
rational number is positive, both the numerator and denominator are
positive, and if the rational number is negative, only the numerator
is negative. |#

(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (cond
               ((and (< n 0) (< d 0)) +)
               ((< n 0) -)
               ((< d 0) -)
               (else +))))
    (cons (sign (abs (/ n g)))
          (+    (abs (/ d g))))))

(define (print-rat x)
  (newline)
  (display (car x))
  (display "/")
  (display (cdr x)))

(print-rat (make-rat 4 -2)) ; => -2/1
(print-rat (make-rat -20 -4)) ; => 5/1
(print-rat (make-rat -19 4)) ; => -19/4
(print-rat (make-rat 1 7)) ; => 1/7
