#lang racket
(define (n2m n m a)   ;;check m>n
  (cond [(= (car a) 0) (append (make-list (- m n) 0) a)]
        [else (append (make-list (- m n) 1) a)]))