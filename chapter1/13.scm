#| Exercise 1.13

Prove that Fib(n) is the closest integer to φ^n/√5, where
φ = (1 + √5)/2.

Hint: Let ψ = (1 - √5)/2. Use induction and the definition of the
Fibonacci numbers (see section 1.2.2) to prove that
Fib(n) = (φ^n - ψ^n)/√5. |#

;;; Solution

;; Fib(n) = (φ^n - ψ^n)/√5
;;
;; Begin the proof by induction with the base cases of n=0 and n=1.
;;
;; Fib(0) = (φ^0 - ψ^0)/√5 = (1 - 1)/√5 = 0
;; Fib(1) = (φ^1 - ψ^1)/√5 = (1 + √5 - 1 + √5) / 2√5 = 2√5 / 2√5 = 1
;;
;; With the base cases established, we now need to show that
;; Fib(n+2) = Fib(n+1) + Fib(n)
;;
;; Start by expanding Fib(n+1) into terms of Fib(n).
;; Fib(n+1) = (φ^(n+1) - ψ^(n+1)) / √5
;;          = (φφ^n - ψψ^n) / √5
;;          = (φ^n + √5φ^n - ψ^n + √5ψ^n) / 2√5
;;          = (φ^n - ψ^n) / 2√5 + (√5φ^n + √5ψ^n) / 2√5
;;          = (1/2) * Fib(n) + (1/2) * (φ^n + ψ^n)
;;
;; Now we can expand Fib(n+2) into terms of Fib(n) and Fib(n+1).
;; Fib(n+2) = (φ^(n+2) - ψ^(n+2)) / √5
;;          = (φφφ^n - ψψψ^n) / √5
;;          = (φφ^n + √5φφ^n - ψψ^n + √5ψψ^n) / 2√5
;;          = (φ^n + √5φ^n + √5φ^n + 5φ^n - ψ^n + √5ψ^n + √5ψ^n - 5ψ^n) / 4√5
;;          = (6φ^n + 2√5φ^n - 6ψ^n + 2√5ψ^n) / 4√5
;;          = (6φ^n - 6ψ^n) / 4√5 + (2√5φ^n + 2√5ψ^n) / 4√5
;;          = 3(φ^n - ψ^n) / 2√5 + (φ^n + ψ^n) / 2
;;          = (3/2) * Fib(n) + (1/2) * (φ^n + ψ^n)
;;          = (1/2) * Fib(n) + (1/2) * (φ^n + ψ^n) + Fib(n)
;;          = Fib(n+1) + Fib(n)
;;
;; To prove that φ^n/√5 is the closest integer to Fib(n), note that the exact
;; formula for Fib(n) has been shown to be (φ^n/√5 - ψ^n/√5). The second term,
;; (ψ^n/√5), is monotonically decreasing as n grows larger. At n=2, the magnitude of this
;; term is already less than one-tenth. Therefore the difference between φ^n/√5 and
;; the exact Fibonacci integer will always be less than a vanishingly small fraction.
;;
;; | φ^n/√5 - Fib(n) | = | ψ^n/√5 | < 0.1 for n > 1
