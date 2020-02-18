#lang racket
(require parser-tools/lex
         parser-tools/yacc)
(require "declarations.rkt")
(provide (all-defined-out))

;main functions being exported are
;maketree and matches?
 


(define (reNumber t l)
  (cond [(Epsilon? t) (Epsilon (+ (Epsilon-n t) l))]
        [(Literal? t) (Literal (Literal-c t) (+ (Literal-n t) l))]
        [(Or? t)
         (let ([t1 (reNumber (Or-t1 t) l)]
               [t2 (reNumber (Or-t2 t) l)])
           (Or t1 t2 (+ (Or-n t) l)))]
        [(Then? t)
         (let ([t1 (reNumber (Then-t1 t) l)]
               [t2 (reNumber (Then-t2 t) l)])
           (Then t1 t2 (+ (Then-n t) l)))]
        [(Star? t)
         (let ([t1 (reNumber (Star-t t) l)])
           (Star t1 (+ (Star-n t) l)))]))

(define (buildTreeHelper r n)
  (cond [(E? r) (cons 1 (Epsilon 1))]
        [(L? r) (cons 1 (Literal (L-c r) 1))]
        [(O? r)
         (let* ([pr1 (buildTreeHelper (O-r1 r) 0)]
                [pr2 (buildTreeHelper (O-r2 r) 0)]
                [t2 (reNumber (cdr pr2) (car pr1))]
                [sub-tree (Or (cdr pr1) t2 (+ 1 (car pr1) (car pr2)))])
           (cons (+ (car pr1) (car pr2) 1) (reNumber sub-tree n)))]
        [(T? r)
         (let* ([pr1 (buildTreeHelper (T-r1 r) 0)]
                [pr2 (buildTreeHelper (T-r2 r) 0)]
                [t2 (reNumber (cdr pr2) (car pr1))]
                [sub-tree (Then (cdr pr1) t2 (+ 1 (car pr1) (car pr2)))])
           (cons (+ (car pr1) (car pr2) 1) (reNumber sub-tree n)))]
        [(S? r)
         (let* ([pr1 (buildTreeHelper (S-r r) 0)]
                [sub-tree (Star (cdr pr1) (+ 1 (car pr1)))])
           (cons (+ (car pr1) 1) (reNumber sub-tree n)))]))




(define (buildTree r)
  (cdr (buildTreeHelper r 0)))

(define (getNodeNumber t)
  (cond [(Epsilon? t) (Epsilon-n t)]
        [(Literal? t) (Literal-n t)]
        [(Then? t) (Then-n t)]
        [(Or? t) (Or-n t)]
        [(Star? t) (Star-n t)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (final-initialise-helper greennode  rednodes)
  (if (> (count (curry equal? greennode) rednodes) 0)
      greennode
      -1))

(define (eof-to-hash character) (if (eof-object? character) #\# character))

(define (search-in-list item list) (> (count (curry equal? item) list) 0))

(define (find-next-state trans state nextchar)
  (cond [(eq? state empty ) empty]
        [else (let* ([item-containing
                      (findf (lambda (arg)
                               (and
                                (equal? (Trans-start arg) state)
                                (equal? (Trans-sym arg) nextchar)))
                             trans)])
                (if item-containing (Trans-final item-containing) empty))]))

(define (matches? graph input-string)
  (let*
      ([symbols (Graph-symbols graph)]
       [trans (Graph-trans graph)]
       [state (Graph-greennode graph)]
       [final (final-initialise-helper state (Graph-rednodes graph))]
       [input (open-input-string input-string)]
       [nextchar (string (eof-to-hash (read-char input)))]
       [driver-helper
        ((lambda (x) (x x))
         (lambda (driver-helper-recursive)
           (lambda ()
             (if (equal? nextchar "#")
                 (if (search-in-list state (Graph-rednodes graph))
                     "Regex and string match"
                     "Regex and string don't match")
                 (if (search-in-list nextchar symbols)
                     (let*
                         ([nextstate (find-next-state trans state nextchar)])
                       (set! state nextstate)
                       (let* ([initialchar nextchar])
                         (set! nextchar (string (eof-to-hash (read-char input))))
                                   
                         ((driver-helper-recursive driver-helper-recursive))))
                     (string-append "This character not in symbols of regex : " nextchar))))))]
       )
    (driver-helper)))

(define-tokens tokens_a (CHAR))
(define-empty-tokens tokens_b (* @ \| EOF \( \) ))

(define simple-regex-lexer
           (lexer
            ((union (char-range "A" "Z")
                    (char-range "a" "z"))
             (token-CHAR lexeme))
            ("*" (token-*))
            ("|" (token-\|))
            ("(" (token-\())
            (")" (token-\)))
            ("@" (token-@))
            ;; recursively calls the lexer which effectively skips whitespace
            (whitespace (simple-regex-lexer input-port))
            ((eof) (token-EOF))))

                 
(define simple-regex-parser
           (parser
            (start regexp)
            (end EOF)
            (error void)
            (tokens tokens_a tokens_b)
            (precs (left \|)
                   (nonassoc *)
                   (nonassoc \( \)))
            (grammar
             (regexp ((regexp \| regexp_withoutor) (O $1 $3))
                     ((regexp_withoutor) $1))
             (regexp_withoutor
              ((@) (E))
              (() void)
              ((regexp_withoutor \( regexp \))
               (if (eq? void $1) $3 (T $1 $3)))
              ((regexp_withoutor CHAR)
               (if (eq? void $1) (L $2) (T $1 (L $2))))
              ((regexp_withoutor \( regexp \) *)
               (if (eq? void $1) (S $3) (T $1 (S $3))))
              ((regexp_withoutor @)
               (if (eq? void $1) (E) (T $1 (E))))
              ((regexp_withoutor CHAR *)
               (if (eq? void $1) (S (L $2)) (T $1 (S (L $2)))))))))

(define (lex-this lexer input) (lambda () (lexer input)))

(define (regex-parser regex-string)
  (simple-regex-parser
   (lex-this simple-regex-lexer (open-input-string regex-string))))

(define (maketree regexp-string)
  (let* ([regexp (regex-parser regexp-string)]
         [regexpWithHash (T regexp (L "#"))]
         [tree (buildTree regexpWithHash)])
    tree))

