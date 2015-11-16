;;;; CS 230: Discrete Math
;;;; Professor Donald
;;;; Samantha Washko
;;;; Jackson Eckert
;;;; 11/16/15

;;;; Problem Set 5: Sets and Functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 1: Finite Sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. Cartesian Product
; A × B = { (a, b)| a ∈ A, b ∈ B }
(require racket/base)
(define remove-duplicates
  (lambda ((lst <list>))
    (foldl (lambda ((x <list>) (L <list>))
             (cond ((member x L) L)
                   (else (append L (list x)))))
           '() lst)))
(define cart
  (lambda ((A <list>) (B <list>))
    (letrec ((iter
              (lambda ((answer <list>) (C <list>))
                     (cond ((null? C) answer)
                           ((null? answer)
                            (iter (map (lambda (b)
                                         (list (car C) b)) B) (cdr C)))
                           (else (iter (append answer
                                               (map (lambda (b)
                                                      (list (car C) b)) B))
                                       (cdr C)))))))
      (remove-duplicates (iter '() A)))))

; Test Cases
(define A '(1 2 3))
(define B '(4 5 6))
(cart A B) ;((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6))

(define C '(7 8 7))
(define D '(9 0 1))
(cart C D) ;((7 9) (7 0) (7 1) (8 9) (8 0) (8 1))


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

; Test Cases
(define T '((1 2) (3 4) (5 6)))
(fcn? T) ;#t
(define U '((7 8) (9 0) (7 2)))
(fcn? U) ;#f
(define V '((a b) (b c) (b d)))
(fcn? V) ;#f


; 3. Cartesian Product Projections
; Given a Cartesian product A × B the projections π1 and π2 are π1 : A × B → A
; and π2 : A × B → B. If (a, b) ∈ A × B, we have π1(a, b) = a and π2(a, b) = b.
; Functions pi1 and pi2, given an argument set S ⊂ A×B, compute π1(S) and π2(S)
(define pi1
  (lambda ((S <list>))
    (remove-duplicates (map car S))))
(define pi2
  (lambda ((S <list>))
    (remove-duplicates (map cadr S))))

; Test Cases
(define W '((1 2) (3 4) (5 6)))
(pi1 W) ;(1 3 5)
(pi2 W) ;(2 4 6)


; 4. Set Diagonal
; Given a set A, the diagonal of A is
; diag(A) = {(a,a) | a ∈ A} ⊂ A×A.
; Scheme function diag computes the diagonal of a set A.
(define diag
  (lambda ((A <list>))
    (if (null? A)
        A
        (cons (list (car A) (car A))
              (diag (cdr A))))))

; Test Cases
(define a '())
(define b '(1 2 3 4 5 6 7))
(diag a) ; ()
(diag b) ; ((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7))


; 5. Checking Diagonal Relations
; We say a set ∆ is a diagonal set if ∆ = diag(A) for some set A.
; Scheme predicate diag?, given a set D of ordered pairs, returns #t when
; D is a diagonal set, and #f otherwise.
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
(diag? d1) ;#t
(diag? d2) ;#f


; 6. Inverting Diag
; Scheme predicate diag-inv, given a diagonal set ∆, returns a set A
; such that diag(A) = ∆.
(define diag-inv
  (lambda ((Delta <list>))
    (if (null? Delta)
        Delta
        (cons (car (car Delta))
              (diag-inv (cdr Delta))))))

; Test Cases
(define del1 '((1 1) (3 3) (4 4) (8 8)))
(diag-inv del1) ;(1 3 4 8)


; 7. Power Sets
; (a) If A has 2 elements, show P(A) is the set of all subsets of A.
;     Let A be the set (a b). The subsets which can be formed from A are (), (a), (b),
;     (a b). If P(A) is the set of all subsets of A, P(A) = (() (a) (b) (a b)), so
;     P(A) has 4 elements.

; (b) If A has 1 element, P(A) has 2 elements, since the subsets that can be formed
;     from A = (a) are () and (a), so P(A) = (() (a))

; (c) If A has 3 elements, A = (a b c), P(A) = (() (a) (b) (c) (a b) (b c) (a c)
;     (a b c)), so it has 8 elements.

; (d) If A has no elements, A is the empty set (), so P(A) contains the empty set,
;     P(A) = (()), so it has 1 element.

; (e) Given a set A, powerset returns its power set
(require racket/base)
(define remove-duplicates
  (lambda ((lst <list>))
    (foldl (lambda ((x <list>) (L <list>))
             (cond ((member x L) L)
                   (else (append L (list x)))))
           '() lst)))
(define powerset
  (lambda ((A <list>))
    (remove-duplicates
     (letrec ((iter
               (lambda ((answer <list>) (lst <list>))
                      (if (null? lst) answer
                          (iter
                           (append
                            (map (lambda (x) (cons (car lst) x)) answer)
                            answer)
                           (cdr lst))))))
       (iter '(()) A)))))

(powerset '(1 2 3)) ;((3 2 1) (3 2) (3 1) (3) (2 1) (2) (1) ())
(powerset '()) ;(())
(powerset '(4 5 4 6)) ;((6 4 5 4) (6 4 5) (6 4 4) (6 4) (6 5 4) (6 5) (6) (4 5 4)
; (4 5) (4 4) (4) (5 4) (5) ())

; (f) If the set A has n elements, the power set of A has 2^n elements.

; (g) Proof by Induction: see attached document

; (h) P(A) is called the power set of set A of length n because it consists of 2 to
;     the power of n elements.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2: Infinite Sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper Stream Code
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
(define stream-ref
  (lambda ((S <stream>) (n <integer>))
    (if (zero? n) (stream-first S)
        (stream-ref (stream-rest S) (- n 1)))))

; Test Cases
(define E (stream-cart integers integers))
(stream->listn E 10) ;((1 1) (1 2) (2 1) (1 3) (2 2) (3 1) (1 4) (2 3) (3 2) (4 1))
(define F (add-streams ones integers))
(stream->listn F 10) ;(2 3 4 5 6 7 8 9 10 11)
(stream->listn (stream-cart F integers) 10)
;((2 1) (2 2) (3 1) (2 3) (3 2) (4 1) (2 4) (3 3) (4 2) (5 1))


; 2. Functions with Infinite Streams
; It is not possible to check whether or not an inifinite set, represented by a stream,
; is a function, because it would require checking every element to see if there are
; duplicates in the leading values of the ordered pairs, so you can't have delayed
; evaluation. If there is duplicate that would mean the set is not representative of a
; function, a procedure would be able to find it, but if there is not, it would just
; run forever and never halt, as it would never know for sure to return true unless it
; checked every element. We could reduce this to the halting problem, which we know to
; be unsolvable, because we essentially want to know whether the function will halt
; with #f (when it finds a duplicate) or runs forever (if there are not duplicates).


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

; Test Cases
(define X
  (letrec ((iter
            (lambda (A B) (stream-cons (list (stream-first A) (stream-first B))
                                       (iter (stream-rest A) (stream-rest B))))))
    (iter integers ones)))
(stream->listn X 10) ;((1 1) (2 1) (3 1) (4 1) (5 1) (6 1) (7 1) (8 1) (9 1) (10 1))
(stream->listn (str-pi1 X) 10) ;(1 2 3 4 5 6 7 8 9 10)
(stream->listn (str-pi2 X) 10) ;(1 1 1 1 1 1 1 1 1 1)


; 4. Diagonal of Infinite Sets
(define stream-diag
  (lambda ((A <stream>))
    (if (stream-empty? A)
        A
        (stream-cons (list (stream-first A) (stream-first A))
                     (stream-diag (stream-rest A))))))

; Test Cases
(define N (stream-diag integers))
(stream->listn N 10) ;((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9) (10 10))
(define O (stream-diag ones))
(stream->listn O 10) ;((1 1) (1 1) (1 1) (1 1) (1 1) (1 1) (1 1) (1 1) (1 1) (1 1))


; 5. Checking Diagonal Relations of Infinite Sets
; It isn't possible to write a function that determines whether a given set D of ordered pairs is a
; diagonal set when that set is countably infinite. If such a function existed, it would need to be
; able to run through an infinite number of items and check each one, then return true if and only
; if each element satisfies the condition. Logically speaking, this is not possible for an infinite
; set of numbers, and furthermore this would be akin to solving the Halting Problem for this set.
; In other words, this function will never finish, and therefore this function will not perform as
; intended.


; 6. Inverting Diag for Infinite Sets
; diag-inv implemented with streams
(define stream-diag-inv
  (lambda ((Delta <stream>))
    (if (stream-empty? Delta)
        Delta
        (stream-cons (car (stream-first Delta))
                     (stream-diag-inv (stream-rest Delta))))))

; Test Cases
(define P (stream-diag-inv (stream-diag integers)))
(stream->listn P 10) ;(1 2 3 4 5 6 7 8 9 10)


; 7. Power Sets of Infinite Sets
; The power set of a countably infinite set is uncountably infinite. To prove this,
; we show that the size of P(A) is greater than the size of the natural numbers (that
; we cannot find a surjective function to map N onto the power set)
; If we make a table of the elements of the power set against the natural numbers, with
; 1 meaning the number is in the set and 0 meaning it is not, we can see this:
;             |1 2 3 4 5 6 7 8...
; (1)         |1 0 0 0 0 0 0 0...
; (2)         |0 1 0 0 0 0 0 0...
; (2 1)       |1 1 0 0 0 0 0 0...
; (3 2 1)     |1 1 1 0 0 0 0 0...
; (6 4 3 1)   |1 0 1 1 0 1 0 0...
; (7 6 5 3 2) |0 1 1 0 1 1 1 0...
; (6 5 2)     |0 1 0 0 1 1 0 0...
; (7 4 3 1)   |1 0 1 1 0 0 1 0...
; ...
; For however many sets we have in the table, we can add another that is not included,
; using diagonalization, going down the diagonal and flipping 0s to 1s and vice versa.
; This new set cannot already be in the table because it differs from all current sets
; in at least one place. For the current table, we can add the set that corresponds to
; 0 0 1 1 1 0 1 1, which is (8 7 5 4 3). Since P(A) is inifinte, we can continue to add
; sets that are not in the table forever. Therefore, we cannot map the natural numbers
; onto the power set, so it is uncountably infinite.
; A stream must be countably infinite, so we cannot represent the power set of an
; infinite set as a stream.