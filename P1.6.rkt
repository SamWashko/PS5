(require racket/base)
; 6. Inverting diag
; Write a Scheme predicate diag-inv that, given a diagonal set ∆, returns a set A
; such that diag(A) = ∆.

; Part 1: Finite sets
(define diag-inv
  (lambda ((Delta <list>))
    (if (null? Delta)
        Delta
        (cons (car (car Delta))
              (diag-inv (cdr Delta))))))

; Test Cases
(define del1 '((1 1) (3 3) (4 4) (8 8)))

(diag-inv del1)

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

(define stream-diag
  (lambda ((A <stream>))
    (if (stream-empty? A)
        A
        (stream-cons (list (stream-first A) (stream-first A))
                     (stream-diag (stream-rest A))))))

; diag-inv implemented with streams
(define stream-diag-inv
  (lambda ((Delta <stream>))
    (if (stream-empty? Delta)
        Delta
        (stream-cons (car (stream-first Delta))
                     (stream-diag-inv (stream-rest Delta))))))

; Test Cases
(define P (stream-diag-inv (stream-diag integers)))
(stream->listn P 10)
