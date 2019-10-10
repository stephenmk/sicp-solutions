#| Exercise 2.5

Show that we can represent pairs of nonnegative integers using only
numbers and arithmetic operations if we represent the pair a and b as
the integer that is the product 2^a 3^b.  Give the corresponding
definitions of the procedures `cons', `car', and `cdr'.

|#

;;; 2 and 3 are mutually prime, so the resultant product 2^a 3^b can
;;; always be factored back into 2s and 3s. The number of factors
;;; gives us `a' and `b.'

(define (my-cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (my-car x)
  (define (remove-factor z a)
    (if (= (remainder z 2) 0)
        (remove-factor (/ z 2) (+ a 1))
        a ))
  (remove-factor x 0))

(my-car (my-cons 7 9)) ; => 7

(define (my-cdr x)
  (define (remove-factor z b)
    (if (= (remainder z 3) 0)
        (remove-factor (/ z 3) (+ b 1))
        b ))
  (remove-factor x 0))

(my-cdr (my-cons 7 9)) ; => 9
