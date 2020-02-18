#lang racket

(define (s-mult n x y)
 (cond [(and (= (car y) 0) (= (car x) 1)) (append '(1) (s-multh n x y n))]
       [(= (car x) 0) (append '(1) (s-multh n y x n))]
       [else (append '(0) (s-multh n x y n))]))

(define (s-multh n x y p)
  (define (mult l a p)
  (if (= a 1) (app-z (shift-l l (- p 1)) (- (+ n 1) p) (car l)) (make-list (* 2 n) 0)))
  (cond [(= p 1) (mult x (car y) p)]
        [else (s-add (* 2 n) [cond [(and (= (car y) 1) (= p n)) (2scomp x n)]
                                         [else (mult x (car y) p)]] (s-multh n x (cdr y) (- p 1)))]))
(define (s-add n a b)
 (reverse (add n (reverse a) (reverse b) 0)))
(define (add n x y q)
   (cond [(= n 0) null]
  [else (cons (modulo (+ (car x) (car y) (if (= n 1) (- 0 q) q)) 2) (add (- n 1) (cdr x) (cdr y) (quotient (+ (car x) (car y) (if (= n 1) (- 0 q) q)) 2)))]))
(define (shift-l l n)
  (append l (make-list n 0)))
(define (app-z l m z)
  (append (make-list m z) l))
(define (2scomp l n)
 (shift-l (2s l n n) (- n 1)))
(define (2s l n p)
  (cond [(= p 1) (make-list (+ n 1) (car l))]
        [else (s-add (+ n 1) [if (= p n) (append '(0) (shift-l (list (car l)) (- p 1)))
                                 (app-z (shift-l (list (car l)) (- p 1)) (- n (- p 1)) (car l))] (2s (cdr l) n (- p 1)))]))