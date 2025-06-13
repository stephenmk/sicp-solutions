#| Exercise 2.60

We specified that a set would be represented as a list with no
duplicates. Now suppose we allow duplicates. For instance, the set
{1,2,3} could be represented as the list `(2 3 2 1 3 2 2)'. Design
procedures `element-of-set?', `adjoin-set', `union-set', and
`intersection-set' that operate on this representation. How does the
efficiency of each compare with the corresponding procedure for the
non-duplicate representation? Are there applications for which you
would use this representation in preference to the non-duplicate one?
|#

;;; Solution

;; `element-of-set?' requires no changes. Since `set' now may contain
;; a number of duplicates, it will require an extra number of steps
;; proportional to the duplicated elements.
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; `adjoin-set' can now just cons the new element without checking to
;; see if it is already in the set. Since it no longer calls `element-of-set?',
;; it runs in constant time rather than O(n).
(define (adjoin-set x set)
  (cons x set))

;; `intersection-set' requires no changes. Since the sets may contain duplicates,
;; it will have O((n+k)^2) growth where k is the number of duplicates.
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; `union-set' can now merely append the sets, so it no longer grows
;; as O(n^2).
(define (union-set set1 set2)
  (append set1 set2))

;; Since `union-set' and `adjoin-set' are much more efficient with
;; this representation, it may be preferable for applications that
;; make heavy use of these procedures in comparison to the others.

;;; Tests

(element-of-set? 'x '())      ;Value: #f
(element-of-set? 'x '(y))     ;Value: #f
(element-of-set? 'x '(x y))   ;Value: #t
(element-of-set? 'x '(x x y)) ;Value: #t

(define test1 '(a b c a b c))
(define test2 '(a b c x y z))

(adjoin-set 'x test1) ;Value: (x a b c a b c)
(adjoin-set 'x test2) ;Value: (x a b c x y z)

(element-of-set? 'x (intersection-set test1 test2)) ;Value: #f
(element-of-set? 'x (union-set test1 test2))        ;Value: #t

(element-of-set? 'a (intersection-set test1 test2)) ;Value: #t
(element-of-set? 'a (union-set test1 test2))        ;Value: #t

(element-of-set? 'n (intersection-set test1 test2)) ;Value: #f
(element-of-set? 'n (union-set test1 test2))        ;Value: #f

