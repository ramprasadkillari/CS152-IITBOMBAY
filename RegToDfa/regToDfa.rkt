#lang racket
(require parser-tools/lex
         parser-tools/yacc)
(require "declarations.rkt")
(require "utilities.rkt")


(define (null-h t)
  (cond [(Epsilon? t) #t]
        [(Literal? t) #f]
        [(Star? t) #t]
        [(Then? t) (and (null-h (Then-t1 t)) (null-h (Then-t2 t)))]
        [(Or? t) (or (null-h (Or-t1 t)) (null-h (Or-t2 t)))]
       ))
(define (buildNullable t)
  (cond [(Epsilon? t) (list (cons (Epsilon-n t) #t))]
        [(Then? t) [cons (cons (Then-n t) (null-h t)) [append (buildNullable (Then-t1 t)) (buildNullable (Then-t2 t))]]]
        [(Or? t) [cons (cons (Or-n t) (null-h t)) [append (buildNullable (Or-t1 t)) (buildNullable (Or-t2 t))]]]
        [(Star? t) [cons (cons (Star-n t) #t) (buildNullable (Star-t t))]]
        [(Literal? t) (list (cons (Literal-n t) #f))]))
(define (First-h t)
  (cond [(Epsilon? t) null]
        [(Literal? t) (list (Literal-n t))]
        [(Star? t) (First-h (Star-t t))]
        [(Then? t) (let* [(q (Then-t1 t))] (if (null-h q) (append (First-h q) (First-h (Then-t2 t))) (First-h q)))]
        [(Or? t) (append (First-h (Or-t1 t)) (First-h (Or-t2 t)))]
       ))
(define (buildFirst t)
 (cond  [(Epsilon? t) (list (cons (Epsilon-n t) null))]
        [(Then? t) [cons (cons (Then-n t) (First-h t)) [append (buildFirst (Then-t1 t)) (buildFirst (Then-t2 t))]]]
        [(Or? t) [cons (cons (Or-n t) (First-h t)) [append (buildFirst (Or-t1 t)) (buildFirst (Or-t2 t))]]]
        [(Star? t) [cons (cons (Star-n t) (First-h (Star-t t))) (buildFirst (Star-t t))]]
        [(Literal? t) (list (cons (Literal-n t) (First-h t)))]))
(define (Last-h t)
  (cond [(Epsilon? t) null]
        [(Literal? t) (list (Literal-n t))]
        [(Star? t) (Last-h (Star-t t))]
        [(Then? t) (let* [(r (Then-t2 t))] (if (Star? r) (append (Last-h (Then-t1 t)) (Last-h r)) (Last-h r)))]
        [(Or? t) (append (Last-h (Or-t1 t)) (Last-h (Or-t2 t)))]
       ))
(define (buildLast t)
  (cond [(Epsilon? t) (list (cons (Epsilon-n t) null))]
        [(Then? t) [cons (cons (Then-n t) (Last-h t)) [append (buildLast (Then-t1 t)) (buildLast (Then-t2 t))]]]
        [(Or? t) [cons (cons (Or-n t) (Last-h t)) [append (buildLast (Or-t1 t)) (buildLast (Or-t2 t))]]]
        [(Star? t) [cons (cons (Star-n t) (Last-h (Star-t t))) (buildLast (Star-t t))]]
        [(Literal? t) (list (cons (Literal-n t) (Last-h t)))]))

(define (Follow-h t l)
  (cond [(Epsilon? t) null]
        [(Literal? t) [if (null? l) '() (list (cons (Literal-n t) (remove-duplicates l)))]]
        [(Then? t) (let*[(p (Then-t2 t))] (append (Follow-h (Then-t1 t) (let* [(s (First-h p))] (if (null-h p) (append s l) s))) (Follow-h p l)))]
        [(Or? t) (append (Follow-h (Or-t1 t) l) (Follow-h (Or-t2 t) l))]
        [(Star? t) (Follow-h (Star-t t) (append (First-h t) l))]))

(define (buildFollow t)
  (Follow-h t null))

(define (sym-no.s t)
  (cond [(Epsilon? t) null]
        [(Literal? t) (list (cons (Literal-c t) (Literal-n t)))]
        [(Then? t) (append (sym-no.s (Then-t1 t)) (sym-no.s (Then-t2 t)))]
        [(Or? t) (append (sym-no.s (Or-t1 t)) (sym-no.s (Or-t2 t)))]
        [(Star? t) (sym-no.s (Star-t t))]))

(define (help l sns)
  (define (snsh sns)
   (cond [(null? sns) null]
         [(equal? (car l) (caar sns)) (snsh (cdr sns))]
         [else (cons (car sns) (snsh (cdr sns)))]))
 (define (first sns)
    (cond [(null? sns) null]
          [(equal? (car l) (caar sns)) (cons (cdar sns) (first (cdr sns)))]
          [else (first (cdr sns))]))

  (cond [(null? sns) (list (list (car l) (cdr l)))]
          [else [cons (cons (car l) (cons (cdr l) (first sns))) (snsh sns)]]))
(define (sym-nums z)

  (cond [(null? z) null]
        [(not (list? (car z))) (sym-nums (help (car z) (cdr z)))]
        [else (cons (car z) (sym-nums (cdr z)))]))
(define (search n l)
  (cond [(equal? (caar l) n) (cdar l)]
        [else (search n (cdr l))]))
(define (search2 n l)
  (cond [(equal? (cdar l) n) (caar l)]
        [else (search2 n (cdr l))]))

(define (Firstpos n t) (search n (buildFirst t)))
(define (Lastpos n t) (search n (buildLast t)))
(define (Nullable n t) (search n (buildNullable t)))
(define (Followpos n t) (search n (buildFollow t)))
(define (Symbol n t) (search2 n (sym-no.s t)))


(define (symbols-fr-lon l t)
  (define (h l)
    (cond [(null? l) null]
          [else (cons (cons (Symbol (car l) t) (car l)) (h (cdr l)))]))  
  (sym-nums (h l)))
(define (symbols-fr-lop l)
  (cond [(null? l) null]
        [else (cons (caar l) (symbols-fr-lop (cdr l)))]))

(define (qsort l)
 (define (lows x h)
   (cond [(null? x) h]
         [(= (car x) (car l)) (lows (cdr x) h)]
         [else (if (> (car l) (car x)) (lows (cdr x) (cons (car x) h)) (lows (cdr x) h))]))
  (define (highs x h)
    (cond [(null? x) h]
          [(= (car x) (car l)) (highs (cdr x) h)]
          [else (if (< (car l) (car x)) (highs (cdr x) (cons (car x) h)) (highs (cdr x) h))]))
  (cond [(null? l) null]
        [else (append (qsort (lows l null)) (list (car l)) (qsort (highs l null)))]))

(define (buildGraph reg) 

  (define t (maketree reg))
  (define (trans l)
  (define (trans-h l1)
    (cond [(null? l1) null]
          [(equal? (caar l1) "#") null]
          [else (cons [Trans l (caar l1) (qsort (append* (map (lambda (x) (Followpos x t)) (cdar l1))))] [trans-h (cdr l1)])]))
  (trans-h (symbols-fr-lon l t)))

(define (transformed-nodes l)
  (map (lambda (x) (Trans-final x)) (trans l)))

(define greennode (cdar (buildFirst t)))
(define symbols (remove-duplicates (symbols-fr-lop (sym-no.s t))))

(define l greennode)

(define (Transmn l)
               (append* (map  (lambda (x) (trans x)) l)))
(define (node-h l cl)
  (cond [(null? l) (remove-duplicates cl)]
        [else (node-h (append* (map (lambda (y) (cond [(member y cl) null] [else (transformed-nodes y)])) l)) (append cl l))]))

(define nodes
  (node-h (list greennode) null))
(define (rednode-h l)
    (cond [(null? l) null]
          [(member (- (Then-n t) 1) (car l)) (cons (car l) (rednode-h (cdr l)))]
          [else (rednode-h (cdr l))]))
(define rednodes
  (reverse (rednode-h nodes)))
 (Graph greennode nodes (Transmn nodes)  rednodes symbols))

