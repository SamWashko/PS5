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
; For the same reason as problem 2 part 2, this function does not exist in Scheme when implemented
; with streams. I'm gonna flesh out this exp tomorrow after office hours.