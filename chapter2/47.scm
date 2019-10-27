#| Exercise 2.47

Here are two possible constructors for frames:

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

For each constructor supply the appropriate selectors to produce an
implementation for frames. |#

;;; Solution

;;; Selectors for first constructor. Using the naming convention from the book.
(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

;; Test
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define test-frame (make-frame 1 2 3))
(origin-frame test-frame) ;Value: 1
(edge1-frame test-frame) ;Value: 2
(edge2-frame test-frame) ;Value: 3



;;; Selectors for second constructor
(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))

;; Test
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define test-frame (make-frame 1 2 3))
(origin-frame test-frame) ;Value: 1
(edge1-frame test-frame) ;Value: 2
(edge2-frame test-frame) ;Value: 3
