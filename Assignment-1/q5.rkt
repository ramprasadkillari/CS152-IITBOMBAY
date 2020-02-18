#lang racket
(define (u-mult n x y)
  (u-multh n x y n))

(define (u-multh n x y p)
  (define (mult l a p)
  (if (= a 1) (app-z (shift-l l (- p 1)) (- (+ n 1) p)) (make-list (* 2 n) 0)))
  (if (= p 1) (mult x (car y) p) (u-add (* 2 n) (mult x (car y) p) (u-multh n x (cdr y) (- p 1)))))

(define (u-add n a b)
  (reverse (add n (reverse a) (reverse b) 0)))
(define (add n x y q)
   (cond [(= n 0) null]
  [else (cons (modulo (+ (car x) (car y) q) 2) (add (- n 1) (cdr x) (cdr y) (quotient (+ (car x) (car y) q) 2)))]))
(define (shift-l l n)
  (append l (make-list n 0)))
(define (app-z l m)
  (append (make-list m 0) l))