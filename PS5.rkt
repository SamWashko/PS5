; Part 1: Finite Sets

; 1. Cartesian Product
; A × B = { (a, b)| a ∈ A, b ∈ B }
(require racket/base)
(define remove-duplicates
  (lambda ((lst <list>))
    (foldl (lambda (x L)
             (cond ((member x L) L)
                   (else (cons x L))))
           '() lst)))
(define cart
  (lambda ((A <list>) (B <list>))
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
; A × B = { (a, b)| a ∈ A, b ∈ B } implemented with streams
(define stream-cart
  (lambda ((s1 <stream>) (s2 <stream>))
    (letrec ((iter
              (lambda ((x <integer>) (y <integer>) (i <integer>))
                     (if (> x i)
                         (iter 0 (+ i 1) (+ i 1))
                         (stream-cons (list (stream-ref s1 x) (stream-ref s2 y))
                                      (iter (+ x 1) (- y 1) i))))))
      (iter 0 0 0))))

; Test Cases
(define E (stream-cart integers integers))
(stream->listn E 10) ;((1 1) (1 2) (2 1) (1 3) (2 2) (3 1) (1 4) (2 3) (3 2) (4 1))
(define F (add-streams ones integers))
(stream->listn F 10) ;(2 3 4 5 6 7 8 9 10 11)
(stream->listn (stream-cart F integers) 10) ;((2 1) (2 2) (3 1) (2 3) (3 2) (4 1) (2 4) (3 3) (4 2) (5 1))


  
                     