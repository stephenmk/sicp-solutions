#| Exercise 2.35

Redefine `count-leaves' from section 2.2.2 as an accumulation:

    (define (count-leaves t)
      (accumulate <??> <??> (map <??> <??>)))

|#

;;; Solution

(define test-list '((1 2) 3 4 (5 6 (7 8))))

;; My first attempt is not quite in the format specified by the
;; problem description.
(define (count-leaves t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (accumulate + 0 (map count-leaves t)))))

(count-leaves test-list)  ;Value: 8


;; It can be rewritten in the specified format, but it's very ugly.
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (cond ((null? x) 0) ((not (pair? x)) 1) (else (count-leaves x)))) t)))

(count-leaves test-list)  ;Value: 8


;; Maybe it's okay to write it like this.
(define (count-leaves t)
  (define (op x)
    (cond ((null? x) 0)
          ((not (pair? x)) 1)
          (else (count-leaves x))))
  (accumulate + 0 (map op t)))

(count-leaves test-list)  ;Value: 8


;; The remarkable feature of this implementation in comparison to the
;; original procedure is that the accumulation method does not
;; explicitly use car or cdr. Descent into the tree is handled by the
;; recursive calls to the map procedure.
;;
;; So actually the check for null elements is completely unnecessary.

(define (count-leaves t)
  (define (op x)
    (if (pair? x) (count-leaves x) 1))
  (accumulate + 0 (map op t)))

(count-leaves test-list)  ;Value: 8
