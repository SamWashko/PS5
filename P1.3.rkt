; 3. Cartesian Product Projections
; Given a Cartesian product A × B the projections π1 and π2 are π1 : A × B → A
; and π2 : A × B → B. If (a, b) ∈ A × B, we have π1(a, b) = a and π2(a, b) = b.
; Functions pi1 and pi2, given an argument set S ⊂ A×B, compute π1(S) and π2(S):

(define pi1
  (lambda (S)
    (map car S)))

(define pi2
  (lambda (S)
    (map cadr S)))

(define T '((1 2) (3 4) (5 6)))
(pi1 T) ;(1 3 5)
(pi2 T) ;(2 4 6)