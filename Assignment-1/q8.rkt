#lang racket
(define (s-div n x y)
  (cond [(and (= (car x) 1) (= (car y) 1)) (let*[(help (u-div n (2scomp n x) (2scomp n y)))]
                                             (cons (car help) [2scomp n (cdr help)]))]
        [(and (= (car x) 0) (= (car y) 0)) (u-div n x y)]
        [(and (= (car x) 0) (= (car y) 1)) (let*[(help (u-div n x (2scomp n y)))]
                                             (cons (2scomp n (car help)) (cdr help)))]
        [else (let*[(help (u-div n (2scomp n x) y))]
                                             (cons (2scomp n (car help)) [2scomp n (cdr help)]))]))
(define (2scomp n a)
  (define (rev a)
    (cond [(null? a) null]
          [(= (car a) 0) (cons 1 (rev (cdr a)))]
          [else (cons 0 (rev (cdr a)))]))
    (u-add n (rev a) (append (make-list (- n 1) 0) (list 1))))
(define (u-add n a b)
  (reverse (add n (reverse a) (reverse b) 0)))
(define (add n x y q)
   (cond [(= n 0) null]
  [else (cons (modulo (+ (car x) (car y) q) 2) (add (- n 1) (cdr x) (cdr y) (quotient (+ (car x) (car y) q) 2)))]))

(define (u-div n x y)  (ud n x y n null)) 
  (define (ud n x y p q)
       (cond [(= p 0) (cons q (list x))]
       [else (let* [(ap (append (make-list (- p 1) 0) x))
       (sh (shift-l y (- p 1)))]      
        (cond [(gt sh ap) (ud n x y (- p 1) (append q (list 0)))]
              [(equal? sh ap) (cons (append q (list 1) (make-list p 0)) (make-list n 0))]
              [else (ud n (u-sub n ap sh) y (- p 1) (append q (list 1)))]))]))
 
(define (gt b a)
  (if (null? b) #f (cond [(> (car b) (car a)) #t]
                         [(> (car a) (car b)) #f]
                         [else (gt (cdr b) (cdr a))])))
(define (u-sub n a b)
  (if (gt b a) (make-list n 0) (reverse (sub n (reverse a) (reverse b) 0))))
(define (sub n x y q)
   (cond [(= n 0) null]
  [else (let* [(p (- (car x) (car y) q))] (cons (modulo p 2) (sub (- n 1) (cdr x) (cdr y) (if (= p 0) 0 (/ (- 1 p) 2)))))]))
(define (shift-l l n)
  (append l (make-list n 0)))
(define (app-z l m z)
  (append (make-list m z) l))
