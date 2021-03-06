#lang racket

;; ====================
;; Complete the following functions and submit your file to Canvas.
;; ====================
;; Do not change the names of the functions. 
;; Do not change the number of arguments in the functions.
;; If your file cannot be loaded by the Racket interpreted, your submission may be cancelled. Then, submit only code that works.
;; ====================
;; Grading instructions:
;; There is a series of test cases for each function. In order to state that your function "works as described", your output must be similar to the expected one in each case.

;; === combine ===
(define (combineAux l1 l2)
  (if (or (null? l1) (null? l2))
      null
      (map list l1 l2)
  )    
)
(define (combine m1 m2)
  ;;(combineAux '(1 2 3) '(4 5 6)) Testing
  (if (or (null? m1) (null? m2))
      null
      (cons (combineAux (car m1) (car m2)) (combine (cdr m1) (cdr m2)))
  )    
)

(display "=== Combine ===\n")
(combine '((1 2) (3 4)) '((11 12) (13 14))) ;; '(((1 11) (2 12)) ((3 13) (4 14)))
(combine '((1 2 3) (4 5 6) (7 8 9)) '((a b c) (d e f) (g h i))) ;; '(((1 a) (2 b) (3 c)) ((4 d) (5 e) (6 f)) ((7 g) (8 h) (9 i)))

;; === product ===
(define (productAux n b)
  (if (null? b)
      null
      (map (lambda (x) (list n x)) b)
  )
)
(define (product a b)
  ;;(productAux 5 '(1 2 3)) testing
  (if (or (null? a) (null? b))
      null
      (append (productAux (car a) b) (product (cdr a) b))
  )    
)

(display "=== product ===\n")
(product '(1 2 3) '(4 5 6)) ;; '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6))
(product '(a b) '()) ;; '()
(product '(a b) '(c d e)) ;; '((a c) (a d) (a e) (b c) (b d) (b e))

;; === unique ===
(define (unique lst)
  (if (null? lst)
      null
      (cons (car lst) (unique (filter (lambda (x) (not (equal? x (car lst)))) lst)))
  )    
)

(display "=== unique ===\n")
(unique '(1 2 3 4 3 5 2 6 6 7 1 10)) ;; '(1 2 3 4 5 6 7 10)
(unique '(a a b b b c d a c f f g h)) ;; '(a b c d f g h)
(unique '(a b (1 2) 1 2 (1 2) b b a 2)) ;; '(a b (1 2) 1 2)

;; === multiples ===

(define (multiples ls x)
  	(if (null? ls)
            null
            (filter (lambda (y) (= (remainder y x) 0)) ls)
   )         
)

(display "=== multiples ===\n")
(multiples '(2 4 5 6) 2) ;; '(2 4 6)
(multiples '(9 27 8 15 4) 3) ;; (9 27 15)
(multiples '(9 8 17 5) 6) ;; '()

;; === sum ===

(define (sum m)
  	(if (null? m)
            null
            (apply + (map (lambda (x) (apply + x)) m))
   )	
)

(display "=== sum ===\n")
(sum '((1 2) (3 4))) ;; 10
(sum '((11 12) (13 14))) ;; 50 
(sum '((1 2 3) (4 5 6) (7 8 9))) ;; 45
