#| Exercise 2.18

Define a procedure `reverse' that takes a list as argument and returns
a list of the same elements in reverse order:

(reverse (list 1 4 9 16 25))
(25 16 9 4 1)

|#

;; Recursive process
(define (reverse x)
  (if (null? x)
      '()
      (append (reverse (cdr x))
              (list (car x)))))

(reverse (list 1 4 9 16 25))
;;Value: (25 16 9 4 1)


;; Iterative process
(define (reverse x)
  (define (reverse-iter x r)
    (if (null? x)
        r
        (reverse-iter (cdr x) (cons (car x) r))))
  (reverse-iter x '()))

(reverse (list 1 4 9 16 25))
;;Value: (25 16 9 4 1)
