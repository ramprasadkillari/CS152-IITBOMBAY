#lang racket
(define (mult k l a b)
(cond [(or (= (b2u k (cadr a)) 0) (= (b2u k (cadr b)) 0)) (list 0 (make-list k 0) (make-list l 0))]
      [else (let* [(new [* (+ 1 (/ (b2u l (caddr a)) (expt 2 l))) (+ 1 (/ (b2u l (caddr b)) (expt 2 l)))])
               (p (exp new)) 
              (2bcon (+ (b2u k (cadr a))(b2u k (cadr b)) (car p) (- 1 [expt 2 (- k 1)])))]
  (list (signofp a b) (u2b k 2bcon) (u2b l (floor (* [expt 2 l] (cdr p))))))]))
 (define (signofp a b)
   (cond [(and (= (car a) 0) (= (car b) 0)) 0]
         [(and (= (car a) 1) (= (car b) 1)) 0]
         [else 1]))
(define (divide k l a b)
(cond [(= (b2u k (cadr a)) 0) (list 0 (make-list k 0) (make-list l 0))]
      [else (let* [(new (/ (+ 1 (/ (b2u l (caddr a)) (expt 2 l))) (+ 1 (/ (b2u l (caddr b)) (expt 2 l)))))
               (p (exp new)) 
              (2bcon (+ (b2u k (cadr a)) (- 0 (b2u k (cadr b))) (car p) (- [expt 2 (- k 1)] 1)))]
  (list (signofp a b) (u2b k 2bcon) (u2b l (floor (* [expt 2 l] (cdr p))))))]))
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
