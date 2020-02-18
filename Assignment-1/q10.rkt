#lang racket
(define (float2d k l a)
  
  (define bias (- [expt 2 (- k 1)] 1))
  (define p (b2u k (cadr a)))
  (define exp [expt 2 (- (if (= p 0) 1 p) bias)])
  (define c (fix2d l (caddr a)))
 (cond [(and (= p (- [expt 2 k] 1)) (= c 0)) (displayln "infinity")]
       [(and (= p (- [expt 2 k] 1)) (not (= c 0))) (displayln "NaN")]
       [(and (= c 0) (= p 0)) 0]
       [else (* (if (= (car a) 0) exp (* (- 0 1) exp)) (if (= p 0) c (+ 1 c)))]))


(define (d2float k l a)
  (define max [* (expt 2 [- (expt 2 (- k 1)) 1]) (+ 1 (fix2d l (make-list l 1)))])
   (define bias (- [expt 2 (- k 1)] 1))
  (define min [expt 2 (- 1 bias l)])
  (cond [(> a max) (d2float k l max)]
        [(< a min) (d2float k l min)]
        [else (let*[(p (exp (abs a)))]
   (list (if (< a 0) 1 0) (u2b k (+ (car p) bias)) (u2b l (floor (inexact->exact (* [expt 2 l] (cdr p)))))))]))

(define (exp a) (if (< a 1) (expn a 0) (expm a 0)))
(define (expn a k)
  (if (> a 1) (cons k (- a 1)) (expn (* a 2) (- k 1))))
  (define (expm a k)
    (if (< a 2) (cons k (- a 1)) (expm (/ a 2) (+ k 1))))

(define (fix2d n a) (fix n a 0 0))
(define (fix n a p res)
  (cond [(= p (- 0 n)) res]
        [else (+ res (* (* 0.5 [expt 2 p]) (car a)) (fix n (cdr a) (- p 1) res))]))
(define (b2u n a)
  (bu a n 0))
(define (bu a p res)
  (if (= p 0) res (bu (cdr a) (- p 1) (+ res (* (car a) [expt 2 (- p 1)])) )))
(define (u2b n x)
  (if (< x [expt 2 n])
      (cond [(= n 0) null]
  [else (let* [(p [expt 2 (- n 1)])] (cons (quotient x p) (u2b (- n 1) (modulo x p))))])
      (make-list n 1)))
