#| Exercise 2.61

Give an implementation of `adjoin-set' using the ordered
representation. By analogy with `element-of-set?' show how to take
advantage of the ordering to produce a procedure that requires on the
average about half as many steps as with the unordered
representation. |#

;;; Solution
#| The original `adjoin-set' procedure merely cons'd the new element to the set.

    (define (original-adjoin-set x set)
      (if (element-of-set? x set)
          set
          (cons x set)))

We can't just cons the new element to the set anymore; it has to be in
the correct order. |#

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;;; Tests
(adjoin-set 1 '())      ;Value: (1)
(adjoin-set 1 '(4 5 6)) ;Value: (1 4 5 6)
(adjoin-set 9 '(4 5 6)) ;Value: (4 5 6 9)
(adjoin-set 5 '(4 5 6)) ;Value: (4 5 6)
