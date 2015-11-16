(require racket/base)
; 5. Checking diagonal relations
; We say a set ∆ is a diagonal set if ∆ = diag(A) for some set A.
; Write a Scheme predicate diag? that, given a set D of ordered pairs, returns #t when
; D is a diagonal set, and #f otherwise.

; Part 1: Finite sets
(define diag?
  (lambda ((D <list>))
    (cond ((null? D) #t)
          ((not (equal? (car (car D))
                        (cadr (car D))))
           #f)
          (else
           (diag? (cdr D))))))

; Test Cases
(define d1 '((1 1) (2 2) (3 3)))
(define d2 '((1 2) (3 3) (4 4)))

(diag? d1)
(diag? d2)

; Part 2: Countably infinite sets
; It isn't possible to write a function that determines whether a given set D of ordered pairs is a
; diagonal set when that set is countably infinite. If such a function existed, it would need to be
; able to run through an infinite number of items and check each one, then return true if and only
; if each element satisfies the condition. Logically speaking, this is not possible for an infinite
; set of numbers, and furthermore this would be akin to solving the Halting Problem for this set.
; In other words, this function will never finish, and therefore this function will not perform as
; intended.