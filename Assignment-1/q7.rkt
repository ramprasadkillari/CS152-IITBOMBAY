#lang racket
(define (u-div n a b)
  (udiv-count n a b 0))
(define (udiv-count n a b p)
  (cond [(greaterthan b a) (cons (u2b n p) (cons a null))]
        [else (udiv-count n (u-sub n a b) b (+ p 1))]))


  (define (u-sub n a b)
  (if (greaterthan b a) (make-list n 0) (reverse (sub n (reverse a) (reverse b) 0))))
(define (sub n x y q)
   (cond [(= n 0) null]
  [else (let* [(p (- (car x) (car y) q))] (cons (modulo p 2) (sub (- n 1) (cdr x) (cdr y) (if (= p 0) 0 (/ (- 1 p) 2)))))]))
(define (greaterthan b a)
  (if (null? b) #f (cond [(> (car b) (car a)) #t]
                         [(> (car a) (car b)) #f]
                         [else (greaterthan (cdr b) (cdr a))])))
(define (u2b n x)
  (cond [(= n 0) null]
        [else (let* [(p [expt 2 (- n 1)])](cons (quotient x p) (u2b (- n 1) (modulo x p))))]))