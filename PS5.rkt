; Part 1: Finite Sets

; 1. Cartesian Product
; A × B = { (a, b)| a ∈ A, b ∈ B }
(define remove-duplicates
  (lambda (lst)
    (foldl (lambda (x L)
             (cond ((member x L) L)
                   (else (cons x L))))
           '() lst)))
(define cart
  (lambda (A B)
    (letrec ((iter
              (lambda (answer a)
                     (cond ((null? a) answer)
                           ((null? answer)
                            (iter (map (lambda (b) (list (car a) b)) B) (cdr a)))
                           (else (iter (append answer (map (lambda (b) (list (car a) b)) B))
                               (cdr a)))))))
      (remove-duplicates (iter '() A)))))

; Test Cases
(define A '(1 2 3))
(define B '(4 5 6))
(cart A B) ;((3 6) (3 5) (3 4) (2 6) (2 5) (2 4) (1 6) (1 5) (1 4))

(define C '(7 8 7))
(define D '(9 0 1))
(cart C D) ;((8 1) (8 0) (8 9) (7 1) (7 0) (7 9))


; Part 2: Infinite Sets
(require racket/base)
(require racket/stream)
(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))
(define add-streams 
  (lambda ((a <stream>) (b <stream>))
    (cond ((stream-empty? a) b)
          ((stream-empty? b) a)
          (else (stream-cons (+ (stream-first a) (stream-first b))
                             (add-streams (stream-rest a) (stream-rest b)))))))
(define stream->listn
  (lambda ((s <stream>) (n <integer>))
    (cond ((or (zero? n) (stream-empty? s)) '())
          (else (cons (stream-first s)
                      (stream->listn (stream-rest s) (- n 1)))))))

; 1. Cartesian Product with Infinite Sets
; A × B = { (a, b)| a ∈ A, b ∈ B } implemented
(define str-cart

(define E (add-streams ones integers))
;(stream->listn (str-cart integers E) 10)
                     