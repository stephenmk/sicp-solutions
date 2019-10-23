#lang sicp
(#%require sicp-pict)

#| Exercise 2.44

Define the procedure `up-split' used by `corner-split'.
It is similar to `right-split', except that it switches
the roles of `below' and `beside'. |#

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(paint (right-split einstein 4))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (up-split einstein 4))

(define (corner-split painter n)
       (if (= n 0)
           painter
           (let ((up (up-split painter (- n 1)))
                 (right (right-split painter (- n 1))))
             (let ((top-left (beside up up))
                   (bottom-right (below right right))
                   (corner (corner-split painter (- n 1))))
               (beside (below painter top-left)
                       (below bottom-right corner))))))

(paint (corner-split einstein 4))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit einstein 4))