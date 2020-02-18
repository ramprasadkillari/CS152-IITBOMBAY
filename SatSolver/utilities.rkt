
#lang racket

(provide (all-defined-out))


; Type for respresenting clauses
(struct Var (lit) #:transparent)
(struct And (x y) #:transparent)
(struct Or (x y) #:transparent)
(struct Not (e) #:transparent)
(struct Const (bool) #:transparent)

; Parses a (list 1 -2) into (Or (Var 1) (Not (Var 2)))

(define (parseSubExp ls)
  (cond [(null? ls) (error "Given an empty sub-expression")]
        [(null? (cdr ls)) (parseNeg (car ls))]
        [else (Or (parseNeg (car ls))
                  (parseSubExp (cdr ls)))]))

; Parses i to (Var i) and -i to (Not (Var i))
(define (parseNeg num)
  (if (< num 0) (Not (Var (* num -1))) (Var num)))

; Parses full list
; Ex. (list '( 1 2) '(-3 2)) into
; (And (Or (Var 1) (Var 2)) (Or (Not (Var 3)) (Var 2)))
(define (parseExp es)
  (cond [(null? es) (error "Given empty list of expressions")]
        [(null? (cdr es)) (parseSubExp (car es))]
        [else (And (parseSubExp (car es))
                   (parseExp (cdr es)))]))

