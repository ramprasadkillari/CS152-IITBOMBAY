#lang racket
(define (b2u n a)
  (b2u-helper n a 0))
(define (b2u-helper n a res)
  (cond [(null? a) res]
        [else
            (b2u-helper (- n 1) (cdr a) (+ res (* [expt 2 (- n 1)] (car a))))]))
                          ;;;;;;;;;;;;;;;;;;;;;
(define (u2b n x)
  (if (< x [expt 2 n])
      (cond [(= n 0) null]
  [else (let* [(p [expt 2 (- n 1)])] (cons (quotient x p) (u2b (- n 1) (modulo x p))))])
      #f))