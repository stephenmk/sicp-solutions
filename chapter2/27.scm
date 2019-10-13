#| Exercise 2.27

Modify your `reverse' procedure of 2-18 to produce a `deep-reverse'
procedure that takes a list as argument and returns as its value the
list with its elements reversed and with all sublists deep-reversed as
well.  For example,

(define x (list (list 1 2) (list 3 4)))

x
((1 2) (3 4))

(reverse x)
((3 4) (1 2))

(deep-reverse x)
((4 3) (2 1))

|#

;; Procedure definition for recursive process from ex. 2.18
(define (reverse x)
  (if (null? x)
      '()
      (append (reverse (cdr x))
              (list (car x)))))

;; To get deep-reverse, check to see if `car x' is a pair before
;; appending. If so, deep-reverse it first.
(define (deep-reverse x)
  (if (null? x)
      '()
      (append (deep-reverse (cdr x))
              (if (pair? (car x))
                  (list (deep-reverse (car x)))
                  (list (car x))))))


(define x '((1 2) (3 4)))
(deep-reverse x)
;;Value: ((4 3) (2 1))

(define y '((1 2 (3 4)) (5 6)))
(deep-reverse y)
;;Value: ((6 5) ((4 3) 2 1))
