#lang racket
(define (u-add n a b)
  (reverse (add n (reverse a) (reverse b) 0)))
(define (add n x y q)
   (cond [(= n 0) null]
  [else (cons (modulo (+ (car x) (car y) q) 2) (add (- n 1) (cdr x) (cdr y) (quotient (+ (car x) (car y) q) 2)))]))
                                           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (u-sub n a b)
  (if (greaterthan b a) (make-list n 0) (reverse (sub n (reverse a) (reverse b) 0))))
(define (sub n x y q)
   (cond [(= n 0) null]
  [else (let* [(p (- (car x) (car y) q))] (cons (modulo p 2) (sub (- n 1) (cdr x) (cdr y) (if (= p 0) 0 (/ (- 1 p) 2)))))]))
(define (greaterthan b a)
  (if (null? b) #f (cond [(> (car b) (car a)) #t]
                         [(> (car a) (car b)) #f]
                         [else (greaterthan (cdr b) (cdr a))])))