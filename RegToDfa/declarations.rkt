#lang racket

(provide (struct-out E) (struct-out L) (struct-out O) (struct-out T)
         (struct-out S) (struct-out Epsilon) (struct-out Literal) 
         (struct-out Or)(struct-out Then) (struct-out Star) 
         (struct-out Graph) (struct-out Trans))



; Defining a new type for Regular expressions

(struct E() #:transparent)
(struct L(c) #:transparent)
(struct O(r1 r2) #:transparent)
(struct T(r1 r2) #:transparent)
(struct S(r) #:transparent)

; Defining a new type for syntax tree formed out of Regular expressions

(struct Epsilon (n) #:transparent)
(struct Literal (c n) #:transparent)
(struct Or (t1 t2 n) #:transparent)
(struct Then (t1 t2 n) #:transparent)
(struct Star (t n) #:transparent)

;Structure of Graph

(struct Graph(greennode nodes trans  rednodes symbols) #:transparent)

;Structure of a transition

(struct Trans(start sym final) #:transparent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;