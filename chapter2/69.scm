#| Exercise 2.69

The following procedure takes as its argument a list of
symbol-frequency pairs (where no symbol appears in more than one pair)
and generates a Huffman encoding tree according to the Huffman
algorithm. |#

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

#| `make-leaf-set' is the procedure given above that transforms the
list of pairs into an ordered set of leaves. `successive-merge' is the
procedure you must write, using `make-code-tree' to successively merge
the smallest-weight elements of the set until there is only one
element left, which is the desired Huffman tree. (This procedure is
slightly tricky, but not really complicated. If you find yourself
designing a complex procedure, then you are almost certainly doing
something wrong. You can take significant advantage of the fact that
we are using an ordered set representation.) |#

;;; Solution

;; The key to solving this is observing that the "merged leaf" of two
;; leaves is just the code-tree created with those two leaves. So as
;; leaves are pruned from the original leaf set, they are replaced
;; with trees to form the new ordered set.
(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (let ((new-tree (make-code-tree (car leaf-set)
                                      (cadr leaf-set))))
        (successive-merge (adjoin-set new-tree (cddr leaf-set))))))

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

;; Tree decoder
(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit: CHOOSE-BRANCH" bit))))
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

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

;;; Tests

(define test-pairs '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))

(define test-tree (generate-huffman-tree test-pairs))
#|
((leaf a 8)
 ((((leaf h 1) (leaf g 1) (h g) 2) ((leaf f 1) (leaf e 1) (f e) 2) (h g f e) 4)
  (((leaf d 1) (leaf c 1) (d c) 2) (leaf b 3) (d c b) 5)
  (h g f e d c b)
  9)
 (a h g f e d c b)
 17)
|#

(define test-message '(d e a d b e e f))

(decode (encode test-message test-tree) test-tree)
;;Value: (d e a d b e e f)

