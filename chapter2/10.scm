#| Exercise 2.10

Ben Bitdiddle, an expert systems programmer, looks over Alyssa's
shoulder and comments that it is not clear what it means to divide by
an interval that spans zero.  Modify Alyssa's code to check for this
condition and to signal an error if it occurs. |#

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
  (let ((yL (lower-bound y))
        (yU (upper-bound y)))
    (if (or (= yL 0)
            (= yU 0)
            (and (< yL 0) (> yU 0)))
        (error "Divisor interval spans zero:" y)
        (mul-interval x
                      (make-interval (/ 1.0 yU)
                                     (/ 1.0 yL))))))
