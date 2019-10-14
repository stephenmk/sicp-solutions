#| Exercise 2.33

Fill in the missing expressions to complete the
following definitions of some basic list-manipulation operations
as accumulations:

(define (map p sequence)
  (accumulate (lambda (x y) <??>) nil sequence))

(define (append seq1 seq2)
  (accumulate cons <??> <??>))

(define (length sequence)
  (accumulate <??> 0 sequence))

|#

;;; Solution

;; accumulate definition from book
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

#| The book gives the following example:
     (accumlate cons '() (list 1 2 3 4 5)) ;Value: (1 2 3 4 5)
   Instead of returning each element, we want to operate on each one
   with procedure `p' |#

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              '()
              sequence))

(map square '(1 2 3 4)) ;Value: (1 4 9 16)

;; use seq2 as the initial sequence. the accumulate procedure then
;; prepends elements from seq1 in reverse order onto the initial sequence.
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append '(1 2 3 4) '(5 6 7 8)) ;Value: (1 2 3 4 5 6 7 8)

;; if the sequence were a list of `1's, then the operator would merely
;; be the addition (`+') operator. To produce the same result, we can
;; "replace" every element in the sequence with 1.
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length '(1 2 3 4)) ;Value: 4
(length '()) ;Value: 0
