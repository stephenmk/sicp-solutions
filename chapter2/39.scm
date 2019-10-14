#| Exercise 2.39

Complete the following definitions of `reverse' (Exercise 2-18) in
terms of `fold-right' and `fold-left' from Exercise 2-38

          (define (reverse sequence)
            (fold-right (lambda (x y) <??>) nil sequence))

          (define (reverse sequence)
            (fold-left (lambda (x y) <??>) nil sequence))

|#

;;; Solution

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(reverse '(1 2 3 4))
;;Value: (4 3 2 1)


(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(reverse '(1 2 3 4))
;;Value: (4 3 2 1)
