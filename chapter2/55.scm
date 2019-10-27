#| Exercise 2.55

Eva Lu Ator types to the interpreter the expression

          (car ''abracadabra)

To her surprise, the interpreter prints back `quote'.  Explain. |#

;;; Solution

;; From footnote 34, we know that the interpreter expands the
;; quotation mark into a `quote' procedure. So the expression can be
;; equivalently rewritten as follows:

(car '(quote abracadabra))

;; The `car' of the list "(quote abracadabra)" is the symbol "quote," which
;; is why the interpreter prints it.
