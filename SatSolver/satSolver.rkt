#lang racket

(require "utilities.rkt")

(define assign #hash())

; Fill in your code here. Should finally define a function
; called dpll which returns true or false. Should additionally
; store the satisfying assignment in the variable assign.

(define (unit-clause? exp)
  (cond [(null? exp) #f]
        [(null? (cdr exp)) #t]
        [else #f]))
                

(define (u-clauses exp l)
  (cond [(null? exp) l]
        [(unit-clause? (car exp)) (u-clauses (cdr exp) (cons (car exp) l))]
        [else (u-clauses (cdr exp) l)]))

(define (ass-h exp)
  (define (f a) [if (> 0 a) (set! assign (dict-set assign  (- 0 a) #f)) (set! assign (dict-set assign a #t))])
  (cond [(null? (cdr exp)) (f (car exp))]
        [else (begin (f (car exp)) (ass-h (cdr exp)))]))

(define (disjoint? l1 l2)    
  (cond [(null? l1) #t]
        [(member (car l1) l2) #f]
        [else (disjoint? (cdr l1) l2)]))


(define (neg-l l)
  (map (lambda (x) (- 0 x)) l))

(define (unit-propagation exp listofuc)
   (define all-unitmem (append* listofuc))
  (define neg-allunits (neg-l all-unitmem))
  (define (rem-clauses exp l)
    (cond [(null? exp) l]
          [(disjoint? all-unitmem (car exp)) (rem-clauses (cdr exp) (cons (remove* neg-allunits (car exp)) l))]
          [else (rem-clauses (cdr exp) l)]))
   (begin (ass-h all-unitmem) (rem-clauses exp null)))

(define (neg-var l)  ;returns #t if any  two elements found with opp sign
      (cond [(null? l) #f]
            [(member (list (- 0 (caar l))) l) #t]
            [else (neg-var (cdr l))]))
(define (remove-unit exp)
  (let* [(loucs (u-clauses exp null))]
    (cond [(null? loucs) exp]
          [(neg-var loucs) (cons null null)]
          [else (remove-unit (unit-propagation exp loucs))])))

(define (no-neg var and-exp res)   ;;returns #f if sign of var is not same in all claues ,,else returns the new and-exp by removing var from all clauses
  (cond [(null? and-exp) (begin (ass-h (list var)) res)]
        [(member (- 0 var) (car and-exp)) #f]
        [else (no-neg var (cdr and-exp) (if (member var (car and-exp)) res (cons (car and-exp) res)))]))

(define (f or-exp and-exp l)
    (if (null? or-exp) (cons null and-exp)
          (let* [(h (no-neg (car or-exp) (cons l and-exp) null))] (cond [h (cons (car or-exp) (remove* (list l) h))] [else (f (cdr or-exp) and-exp l)]))))

(define (lit-elim exp l)               ;;f returns a pair of (removed variable from (car exp)) and (new (cdr exp) by removing clauses) containing

 (cond [(null? exp) null]
       [else (let* [(a (f (car exp) (cdr exp) l))] (if (null? (car a)) (cons (car exp) (lit-elim (cdr exp) (append (car exp) l))) (lit-elim (cdr a) l)))]))

(define (after-lit-unit exp)
  (cond [(null? exp) #t]
        [(member null exp) #f]
        [else exp]))


(define (dpll-h l)
  (let* [(ru (remove-unit l))
         (le (lit-elim ru null))
         (alu (after-lit-unit le))]
    (cond [(equal? #t alu) #t]
          [(equal? #f alu) #f]
          [else (if (dpll-h (cons (list (caar alu)) alu)) #t (dpll-h (cons (list (- 0 (caar alu))) alu)))])))

(define (Exp2list Exp n)          ;;exp 1 (and 2) (or 3) (var 4)
  (cond [(And? Exp) (cons (Exp2list (And-x Exp) (+ 1 n)) (Exp2list (And-y Exp) n))]
        [(Or? Exp) (if (= n 1) (list (Exp2list Exp 2)) (cons (Exp2list (Or-x Exp) (+ 1 n)) (Exp2list (Or-y Exp) n)))]
        [(Not? Exp) (let* [(a (- 0 (Var-lit (Not-e Exp))))] (if (= n 1) (list (list a)) (if (= n 2) (list a) a)))]
        [(Var? Exp) (let* [(a (Var-lit Exp))] (if (= n 1) (list (list a)) (if (= n 2) (list a) a)))]))


(define (dpll Exp)
  (begin (set! assign #hash()) (if (dpll-h (Exp2list Exp 1)) #t (begin (set! assign #hash()) #f))))


