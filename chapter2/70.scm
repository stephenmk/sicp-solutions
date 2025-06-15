#| Exercise 2.70

The following eight-symbol alphabet with associated relative
frequencies was designed to efficiently encode the lyrics of 1950s rock
songs. (Note that the “symbols” of an “alphabet” need not be
individual letters.)

A    2   GET 2   SHA  3   WAH 1
BOOM 1   JOB 2   NA  16   YIP 9

Use `generate-huffman-tree' (Exercise 2.69) to generate a corresponding
Huffman tree, and use encode (Exercise 2.68) to encode the following
message:

Get a job
Sha na na na na na na na na
Get a job
Sha na na na na na na na na
Wah yip yip yip yip yip yip yip yip yip
Sha boom

How many bits are required for the encoding? What is the smallest
number of bits that would be needed to encode this song if we used a
ﬁxed-length code for the eight-symbol alphabet? |#

;;; Definitions from previous exercises.

;; Leaf selectors
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;; Leaf constructor
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

;; Tree selectors
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; Tree constructor
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;; Tree encoder
(define (encode message tree)
  (define (encode-symbol symbol tree)
    (define (element-of-symbols? symbol tree)
      (memq symbol (symbols tree)))
    (define (encode-1 bits current-branch)
      (cond ((leaf? current-branch) bits)
            ((element-of-symbols? symbol (left-branch current-branch))
             (encode-1 (append bits '(0))
                       (left-branch current-branch)))
            ((element-of-symbols? symbol (right-branch current-branch))
             (encode-1 (append bits '(1))
                       (right-branch current-branch)))
            (else (error "bad symbol: ENCODE-1" symbol))))
    (cond ((leaf? tree)
           (error "bad tree: ENCODE-SYMBOL" tree))
          ((not (element-of-symbols? symbol tree))
           (error "bad symbol: ENCODE-SYMBOL" symbol))
          (else (encode-1 '() tree))))
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; Huffman tree generator
(define (generate-huffman-tree pairs)
  (define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))
  (define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
          (adjoin-set (make-leaf (car pair)   ; symbol
                                 (cadr pair)) ; frequency
                      (make-leaf-set (cdr pairs))))))
  (define (successive-merge leaf-set)
    (if (null? (cdr leaf-set))
        (car leaf-set)
        (let ((new-tree (make-code-tree (car leaf-set)
                                        (cadr leaf-set))))
          (successive-merge (adjoin-set new-tree (cddr leaf-set))))))
  (successive-merge (make-leaf-set pairs)))

;;; Solution

(define test-pairs '((A 2) (GET 2) (SHA 3) (WAH 1)
                     (BOOM 1) (JOB 2) (NA 16) (YIP 9)))

(define test-tree (generate-huffman-tree test-pairs))

(encode '(na) test-tree)   ;Value: (0)
(encode '(yip) test-tree)  ;Value: (1 0)
(encode '(sha) test-tree)  ;Value: (1 1 1 0)
(encode '(a) test-tree)    ;Value: (1 1 0 0)
(encode '(job) test-tree)  ;Value: (1 1 1 1 0)
(encode '(get) test-tree)  ;Value: (1 1 1 1 1)
(encode '(wah) test-tree)  ;Value: (1 1 0 1 1)
(encode '(boom) test-tree) ;Value: (1 1 0 1 0)

(define song (append '(Get a job)
                     '(Sha na na na na na na na na)
                     '(Get a job)
                     '(Sha na na na na na na na na)
                     '(Wah yip yip yip yip yip yip yip yip yip)
                     '(Sha boom)))

(encode song test-tree)
;;Value: (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 0)

(length song)                    ;Value: 36
(length (encode song test-tree)) ;Value: 84

#| For an alphabet with 8 symbols, a fixed-length code would require
log_2(8) = 3 bits per symbol. The song is 36 symbols long, so it would
require 36*3 = 108 bits to encode. The Huffman encoding is more
efficient, requiring only 84 bits. |#
