#lang racket
(define (b2s n a)
  (if (= (car a ) 0) (b2s-helper (- n 1) (cdr a) 0)
  (- (b2s-helper (- n 1) (cdr a) 0) [expt 2 (- n 1)])))
(define (b2s-helper n a res)
  (cond [(null? a) res]
        [else
            (b2s-helper (- n 1) (cdr a) (+ res (* [expt 2 (- n 1)] (car a))))]))
                          ;;;;;;;;;;;;;;;;;;;;;
(define (s2b n x)
  (cond [(and (not (< x (- 0 [expt 2 (- n 1)]))) (< x [expt 2 (- n 1)]))
         (if (< x 0) (cons 1 (s2b-helper (- n 1) (+ x [expt 2 (- n 1)]))) (s2b-helper n x))]
        [else #f]))
(define (s2b-helper n x)
      (cond [(= n 0) null]
  [else (cons (quotient x [expt 2 (- n 1)]) (s2b-helper (- n 1) (modulo x [expt 2 (- n 1)])))]))
      