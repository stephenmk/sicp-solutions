#lang sicp
(#%require sicp-pict)

#| Exercise 2.45

`Right-split' and `up-split' can be expressed as
instances of a general splitting operation.  Define a procedure
`split' with the property that evaluating

          (define right-split (split beside below))
          (define up-split (split below beside))

produces procedures `right-split' and `up-split' with the same
behaviors as the ones already defined. |#

(define (split x y)
  (lambda (painter n)
    (define (split-internal m)
      (if (= m 0)
          painter
          (let ((smaller (split-internal (- m 1))))
            (x painter (y smaller smaller)))))
    (split-internal n)))


(define right-split (split beside below))
(define up-split (split below beside))


(paint (right-split einstein 4))

(paint (up-split einstein 4))