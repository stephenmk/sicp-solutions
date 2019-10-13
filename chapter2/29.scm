#| Exercise 2.29

A binary mobile consists of two branches, a left branch and a right
branch.  Each branch is a rod of a certain length, from which hangs
either a weight or another binary mobile.  We can represent a binary
mobile using compound data by constructing it from two branches (for
example, using `list'): |#

(define (make-mobile left right)
  (list left right))

#| A branch is constructed from a `length' (which must be a number)
together with a `structure', which may be either a number
(representing a simple weight) or another mobile: |#

(define (make-branch length structure)
  (list length structure))

#| a. Write the corresponding selectors `left-branch' and `right-branch',
which return the branches of a mobile, and `branch-length' and
`branch-structure', which return the components of a branch. |#

;;; Solution
(define left-branch car)
(define right-branch (lambda (mobile) (car (cdr mobile))))

(define branch-length car)
(define branch-structure (lambda (branch) (car (cdr branch))))

(define test-mobile (make-mobile (make-branch 1 2) (make-branch 3 4)))
(left-branch test-mobile) ;Value: (1 2)
(right-branch test-mobile) ;Value: (3 4)

(define test-branch (make-branch 1 test-mobile))
(branch-length test-branch) ;Value: 1
(branch-structure test-branch) ;Value: ((1 2) (3 4))

#| b. Using your selectors, define a procedure `total-weight' that
returns the total weight of a mobile. |#

;;; Solution
(define (total-weight mobile)
  (if (pair? mobile)
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
      mobile))

(define test-mobile-2 (make-mobile (make-branch 5 6) test-branch))
;;Value: ((5 6) (1 ((1 2) (3 4)))), weight = 6 + 2 + 4 = 12

(total-weight test-mobile-2)
;;Value: 12

#| c. A mobile is said to be "balanced" if the torque applied by its
top-left branch is equal to that applied by its top-right branch (that
is, if the length of the left rod multiplied by the weight hanging
from that rod is equal to the corresponding product for the right
side) and if each of the submobiles hanging off its branches is
balanced. Design a predicate that tests whether a binary mobile is
balanced. |#

;;; Solution
(define (balanced? mobile)
  (if (not (pair? mobile)) #f
      (let ((lbl (branch-length (left-branch mobile)))
            (lbs (branch-structure (left-branch mobile)))
            (rbl (branch-length (right-branch mobile)))
            (rbs (branch-structure (right-branch mobile))))
        (cond ((and (pair? lbs) (not (balanced? lbs))) #f)
              ((and (pair? rbs) (not (balanced? rbs))) #f)
              ((= (* lbl (total-weight lbs)) (* rbl (total-weight rbs))) #t)
              (else #f)))))

#| d. Suppose we change the representation of mobiles so that the
constructors are |#

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

#| How much do you need to change your programs to convert to
the new representation? |#

;;; Solution
;; Only the selectors would need to be modified, because these have
;; been designed to be the only procedures which directly operate on
;; mobile and branch list-structures. (I also used `pair?', but this
;; usage would be unaffected by the described changes).
