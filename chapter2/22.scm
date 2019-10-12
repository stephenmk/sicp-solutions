#| Exercise 2.22

Louis Reasoner tries to rewrite the first `square-list' procedure of
Exercise 2-21 so that it evolves an iterative process:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

Unfortunately, defining `square-list' this way produces the answer
list in the reverse order of the one desired.  Why?

Louis then tries to fix his bug by interchanging the arguments to
`cons':

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

This doesn't work either.  Explain. |#


#| Solution

The first `square-list' procedure produces a backwards list because
each subsequently calculated square is placed at the beginning of the
`answer' list by the `cons' procedure.

The second procedure uses the `cons' procedure to create pairs from
the `answer' list and a squared value, which is not a list; it is a
pair containing a list and a number. This can be fixed by using `append'
instead of `cons.'

|#

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                     (list (square (car things)))))))
  (iter items '()))

(square-list (list 1 2 3 4))
;Value: (1 4 9 16)
