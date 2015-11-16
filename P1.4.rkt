(require racket/base)
; 4. Set Diagonal
; Given a set A, the diagonal of A is
; diag(A) = {(a,a) | a ∈ A} ⊂ A×A.
; Write a Scheme function diag that computes the diagonal of a set A.

; Part 1: Finite sets
(define diag
  (lambda ((A <list>))
    (if (null? A)
        A
        (cons (list (car A) (car A))
              (diag (cdr A))))))

; Test Cases
(define a '())
(define b '(1 2 3 4 5 6 7))
;(define c '(1 2 3 3))

(diag a) ; ()
(diag b) ; ((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7))

; Part 2: Countably infinite sets
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

; diag(A) implemented with streams
(define stream-diag
  (lambda ((A <stream>))
    (if (stream-empty? A)
        A
        (stream-cons (list (stream-first A) (stream-first A))
                     (stream-diag (stream-rest A))))))

; Test Cases
(define N (stream-diag integers))
(stream->listn N 10)
(define O (stream-diag ones))
(stream->listn O 10)