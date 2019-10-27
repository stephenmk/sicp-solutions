#| Exercise 2.48

A directed line segment in the plane can be represented as a pair of
vectors--the vector running from the origin to the start-point of the
segment, and the vector running from the origin to the end-point of
the segment.  Use your vector representation from Exercise
2-46 to define a representation for segments with a constructor
`make-segment' and selectors `start-segment' and `end-segment'. |#

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

(define (make-segment start-vect end-vect)
  (cons start-vect end-vect))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))
