#| Exercise 2.68

The `encode' procedure takes as arguments a message and a tree and
produces the list of bits that gives the encoded message. |#

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

#| `encode-symbol' is a procedure, which you must write, that returns
the list of bits that encodes a given symbol according to a given
tree. You should design `encode-symbol' so that it signals an error if
the symbol is not in the tree at all. Test your procedure by encoding
the result you obtained in Exercise 2.67 with the sample tree and
seeing whether it is the same as the original sample message. |#

;;; Solution
(define (encode-symbol symbol tree)
  (define (encode-1 bits current-branch)
    (cond ((leaf? current-branch)
           (if (eq? symbol (symbol-leaf current-branch))
               bits
               (error "bad symbol: ENCODE-SYMBOL" symbol)))
          ((memq symbol (symbols (left-branch current-branch)))
           (encode-1 (append bits '(0))
                     (left-branch current-branch)))
          ((memq symbol (symbols (right-branch current-branch)))
           (encode-1 (append bits '(1))
                     (right-branch current-branch)))
          (else (error "bad symbol: ENCODE-SYMBOL" symbol))))
  (encode-1 '() tree))

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

;; Test data
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(a d a b b c a))
;; Expected value:
;;  (0 1 1 0 0 1 0 1 0 1 1 1 0)

(encode sample-message sample-tree)
;; Value:
;;  (0 1 1 0 0 1 0 1 0 1 1 1 0)
