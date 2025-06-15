#| Exercise 2.71

Suppose we have a Huffman tree for an alphabet of n symbols, and that
the relative frequencies of the symbols are 1, 2, 4, ...,
2^(n−1). Sketch the tree for n = 5; for n = 10. In such a tree (for
general n) how many bits are required to encode the most frequent
symbol? The least frequent symbol? |#

;;; Solution

;; Here is the tree for n = 10. The pairs are ((a 1) (b 2) (c 4) (d 8)
;; (e 16) (f 32) (g 64) (h 128) (i 256) (j 512)).
;;
;;                        1023
;;                        /  \
;;                      511   (j 512)
;;                      /  \
;;                    255  (i 256)
;;                    /  \
;;                  127   (h 128)
;;                  /  \
;;                 63   (g 64)
;;                /  \
;;               31   (f 32)
;;              /  \
;;             15   (e 16)
;;            /  \
;;           7    (d 8)
;;          / \
;;         3   (c 4)
;;        / \
;;    (a 1)  (b 2)
;;
;; The subtree starting at 31 would be the tree for n = 5.
;;
;; j => (1)                  (1 bit)
;; i => (0 1)                (2 bits)
;; h => (0 0 1)              (3 bits)
;; g => (0 0 0 1)            (4 bits)
;; f => (0 0 0 0 1)          (5 bits)
;; e => (0 0 0 0 0 1)        (6 bits)
;; d => (0 0 0 0 0 0 1)      (7 bits)
;; c => (0 0 0 0 0 0 0 1)    (8 bits)
;; b => (0 0 0 0 0 0 0 0 1)  (9 bits)
;; a => (0 0 0 0 0 0 0 0 0)  (9 bits)
;;
;; Only one bit is required for the most frequent symbol for all values of n.
;;
;; The least frequent symbol requires n-1 bits, because the least two
;; common symbols will have the same number of bits.
