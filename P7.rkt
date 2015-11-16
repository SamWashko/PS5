; 1.7

; (a) If A has 2 elements, show P(A) is the set of all subsets of A.
;     Let A be the set (a b). The subsets which can be formed from A are (), (a), (b),
;     (a b). If P(A) is the set of all subsets of A, P(A) = (() (a) (b) (a b)), so
;     P(A) has 4 elements.

; (b) If A has 1 element, P(A) has 2 elements, since the subsets that can be formed
;     from A = (a) are () and (a), so P(A) = (() (a))

; (c) If A has 3 elements, A = (a b c), P(A) = (() (a) (b) (c) (a b) (b c) (a c)
;     (a b c)), so it has 8 elements.

; (d) If A has no elements, A is the empty set (), so P(A) contains the empty set,
;     P(A) = (()), so it has 1 element.

; (e) Given a set A, powerset returns its power set
(require racket/base)
(define remove-duplicates
  (lambda ((lst <list>))
    (foldl (lambda ((x <list>) (L <list>))
             (cond ((member x L) L)
                   (else (append L (list x)))))
           '() lst)))
(define powerset
  (lambda ((A <list>))
    (remove-duplicates
     (letrec ((iter
               (lambda ((answer <list>) (lst <list>))
                      (if (null? lst) answer
                          (iter
                           (append
                            (map (lambda (x) (cons (car lst) x)) answer)
                            answer)
                           (cdr lst))))))
       (iter '(()) A)))))

(powerset '(1 2 3)) ;((3 2 1) (3 2) (3 1) (3) (2 1) (2) (1) ())
(powerset '()) ;(())
(powerset '(4 5 4 6)) ;((6 4 5 4) (6 4 5) (6 4 4) (6 4) (6 5 4) (6 5) (6) (4 5 4)
; (4 5) (4 4) (4) (5 4) (5) ())

; (f) If the set A has n elements, the power set of A has 2^n elements.
; (g) Proof by Induction:

; (h) P(A) is called the power set of set A of length n because it consists of 2 to
;     the power of n elements.

; 2.7
; The power set of a countably infinite set is uncountably infinite. To prove this,
; we show that the size of P(A) is greater than the size of the natural numbers (that
; we cannot find a surjective function to map N onto the power set)
; If we make a table of the elements of the power set against the natural numbers, with
; 1 meaning the number is in the set and 0 meaning it is not, we can see this:
;             |1 2 3 4 5 6 7 8...
; (1)         |1 0 0 0 0 0 0 0...
; (2)         |0 1 0 0 0 0 0 0...
; (2 1)       |1 1 0 0 0 0 0 0...
; (3 2 1)     |1 1 1 0 0 0 0 0...
; (6 4 3 1)   |1 0 1 1 0 1 0 0...
; (7 6 5 3 2) |0 1 1 0 1 1 1 0...
; (6 5 2)     |0 1 0 0 1 1 0 0...
; (7 4 3 1)   |1 0 1 1 0 0 1 0...
; ...
; For however many sets we have in the table, we can add another that is not included,
; using diagonalization, going down the diagonal and flipping 0s to 1s and vice versa.
; This new set cannot already be in the table because it differs from all current sets
; in at least one place. For the current table, we can add the set that corresponds to
; 0 0 1 1 1 0 1 1, which is (8 7 5 4 3). Since P(A) is inifinte, we can continue to add
; sets that are not in the table forever. Therefore, we cannot map the natural numbers
; onto the power set, so it is uncountably infinite.
; A stream must be countably infinite, so we cannot represent the power set of an
; infinite set as a stream.