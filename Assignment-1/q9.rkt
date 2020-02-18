#lang racket
(define (fix2d n a) (fix n a n 0))
(define (fix n a p res)
  (cond [(= p (- 0 n)) res]
        [else (+ res (* (* 0.5 [expt 2 p]) (car a)) (fix n (cdr a) (- p 1) res))]))

(define (d2fix n a)
 (let* [(p (inexact->exact a))] (append (u2b n (floor p)) (u2b n (floor (* [expt 2 n] (- p (floor p))))))))
(define (u2b n x)
  (if (< x [expt 2 n])
      (cond [(= n 0) null]
  [else (let* [(p [expt 2 (- n 1)])] (cons (quotient x p) (u2b (- n 1) (modulo x p))))])
      (make-list n 1)))