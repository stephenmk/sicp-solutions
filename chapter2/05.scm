#| Exercise 2.5

Show that we can represent pairs of nonnegative integers using only
numbers and arithmetic operations if we represent the pair a and b as
the integer that is the product 2^a 3^b.  Give the corresponding
definitions of the procedures `cons', `car', and `cdr'. |#


#| Solution

2 and 3 are mutually prime, so the resultant product 2^a 3^b can
always be factored back into 2s and 3s. The number of factors gives us
`a' and `b.' |#

(define (my-cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (count-factors integer factor)
  (define (remove-factor z count)
    (if (= (remainder z factor) 0)
        (remove-factor (/ z factor) (+ count 1))
        count))
  (remove-factor integer 0))

(define (my-car x)
  (count-factors x 2))

(my-car (my-cons 7 9)) ; => 7

(define (my-cdr x)
  (count-factors x 3))

(my-cdr (my-cons 7 9)) ; => 9
