#| Exercise 2.75

Implement the constructor `make-from-mag-ang' in message-passing
style. This procedure should be analogous to the `make-from-real-imag'
procedure given above. |#

;;; Solution

;; `make-from-real-imag' from the book
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)


;; New `make-from-mag-ang' object constructor
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)


;; Tests
(define test (make-from-real-imag 2 2))
(test 'real-part) ;Value: 2
(test 'imag-part) ;Value: 2
(test 'magnitude) ;Value: 2.8284271247461903
(test 'angle)     ;Value: .7853981633974483

(define test2
  (make-from-mag-ang (test 'magnitude)
                     (test 'angle)))

(test2 'real-part) ;Value: 2.0000000000000004
(test2 'imag-part) ;Value: 2.
(test2 'magnitude) ;Value: 2.8284271247461903
(test2 'angle)     ;Value: .7853981633974483
