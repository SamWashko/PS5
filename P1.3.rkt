; 3. Cartesian Product Projections
; Given a Cartesian product A × B the projections π1 and π2 are π1 : A × B → A
; and π2 : A × B → B. If (a, b) ∈ A × B, we have π1(a, b) = a and π2(a, b) = b.
; Functions pi1 and pi2, given an argument set S ⊂ A×B, compute π1(S) and π2(S)
(require racket/base)
(define remove-duplicates
  (lambda ((lst <list>))
    (foldl (lambda ((x <list>) (L <list>))
             (cond ((member x L) L)
                   (else (append L (list x)))))
           '() lst)))

(define pi1
  (lambda ((S <list>))
    (remove-duplicates (map car S))))

(define pi2
  (lambda ((S <list>))
    (remove-duplicates (map cadr S))))

(define W '((1 2) (3 4) (5 6)))
(pi1 W) ;(1 3 5)
(pi2 W) ;(2 4 6)


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

; 3. Cartesian Product Projections with Infinite Sets
; Given an infinite Cartesian product A × B the projections are π1 : A × B → A
; and π2 : A × B → B. If (a, b) ∈ A × B, we have π1(a, b) = a and π2(a, b) = b.
; Functions pi1 and pi2, given an argument set S ⊂ A×B, compute π1(S) and π2(S)

(define str-pi1
  (lambda ((S <stream>))
    (stream-cons (car (stream-first S)) (str-pi1 (stream-rest S)))))
(define str-pi2
  (lambda ((S <stream>))
    (stream-cons (cadr (stream-first S)) (str-pi2 (stream-rest S)))))

; Test Case
(define X
  (letrec ((iter
            (lambda (A B) (stream-cons (list (stream-first A) (stream-first B))
                                       (iter (stream-rest A) (stream-rest B))))))
    (iter integers ones)))
(stream->listn X 10) ;((1 1) (2 1) (3 1) (4 1) (5 1) (6 1) (7 1) (8 1) (9 1) (10 1))
(stream->listn (str-pi1 X) 10) ;(1 2 3 4 5 6 7 8 9 10)
(stream->listn (str-pi2 X) 10) ;(1 1 1 1 1 1 1 1 1 1)