; Part 1: Finite Sets

; 1. Cartesian Product
; A × B = { (a, b)| a ∈ A, b ∈ B }.
(define cart
  (lambda (A B)
    (letrec ((iter
              (lambda (answer a)
                     (cond ((null? a) answer)
                           ((null? answer)
                            (iter (map (lambda (b) (list (car a) b)) B) (cdr a)))
                           (else (iter (append answer (map (lambda (b) (list (car a) b)) B))
                               (cdr a)))))))
      (iter '() A))))

(define C '(1 2 3))
(define D '(4 5 6))
(cart C D) ;((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6))
                     