; 2. Functions
; A function f : A → B can be represented by a set R of ordered pairs,
; where R ⊂ A × B and (a, b) ∈ R iff f(a) = b. An element in the domain
; should be mapped to at most one element of the range.
; Given a set S ⊂ A × B, fcn? returns #t when S represents a function,
; and #f when it does not.

(require racket/base)
(define fcn?
  (lambda ((S <list>))
    (letrec ((iter (lambda ((s <list>))
                     (cond ((null? s) #t)
                           ((foldr (lambda (a b) (or a b)) #f
                                   (map (lambda (coord)
                                          (and (equals? (car coord) (car (car s)))
                                               (not (equals? (cdr coord)
                                                             (cdr (car s)))))) s))
                            #f)
                           (else (iter (cdr s)))))))
      (iter S))))

(define T '((1 2) (3 4) (5 6)))
(fcn? T) ;#t
(define U '((7 8) (9 0) (7 2)))
(fcn? U) ;#f
(define V '((a b) (b c) (b d)))
(fcn? V) ;#f


; Part 2

; 2. Functions with Infinite Streams
; It is not possible to check whether or not an inifinite set, represented by
; a stream, is a function, because it would require checking every element to
; see if there are duplicates in the leading values of the ordered pairs, so
; you can't have delayed evaluation. If there is duplicate that would mean the
; set is not representative of a function, a procedure would be able to find it,
; but if there is not, it would just run forever and never halt, as it would
; never know for sure to return true unless it checked every element.