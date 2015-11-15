; 2. Functions
; A function f : A → B can be represented by a set R of ordered pairs,
; where R ⊂ A × B and (a, b) ∈ R iff f(a) = b. An element in the domain
; should be mapped to at most one element of the range.
; Given a set S ⊂ A × B, fcn? returns #t when S represents a function,
; and #f when it does not.

; Note: I'm not really sure if this is what they're looking for, also this
; is inefficient, I just couldn't get a more efficient way to work

(define fcn?
  (lambda (S)
    (letrec ((iter (lambda (s)
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