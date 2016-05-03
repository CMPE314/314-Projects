#lang racket
(require plai-typed)

;FURKAN ÇELİK
;111200035
;;GRAMMAR
;;numC: number
;;idC: symbol
;;appC: symbol,ExprC
;;plusC: l:ExprC,r:ExprC
;;multC: l:ExprC,r:ExprC
;;factC: l:ExprC
;;ifC: condition:ExprC true:ExprC,false:ExprC
(define-type ExprC
 [numC (n : number)]
 [idC (s : symbol)]
 [appC (fun : symbol) (arg : ExprC)]
 [plusC (l : ExprC) (r : ExprC)]
 [multC (l : ExprC) (r : ExprC)]
 [factC (l : ExprC)]
 [ifC (c : ExprC) (t : ExprC) (f : ExprC)])

;FundefC is a structure of function defination.
;for example you add double factorial etc.
(define-type FunDefC
 [fdC (name : symbol) (arg : s-expression ) (body : ExprC)])


;get-fundef we can say get the functions
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
 (cond
   [(empty? fds) (error 'get-fundef "undefined function")]
   [(cons? fds) (cond
                  [(equal? n (fdC-name (first fds))) (first fds)]
                  [else (get-fundef n (rest fds))])]))

;interpreter is a evaluator
;it makes elaluation things.
;;Contract:
;;ExprC + listof FunDefC --> number
;;Template:
;;define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
;;  (type-case ExprC in
;;    [numC (n) ...]
;;    [idC (_) ...]
;;    [appC (f a) ...]
;;    [plusC (l r) ...]
;;    [multC (l r) ...]
;;    [factC (x) ...]
;;    [ifC (c y n)...])
(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
 (type-case ExprC e
   [numC (n) n]
   [idC (_) (error 'interp "shouldn't get here")]
   [appC (f a) (local ([define fd (get-fundef f fds)])
             (interp (subst a
                            (fdC-arg fd)
                            (fdC-body fd))
                     fds))]  
   [plusC (l r) (+ (interp l fds) (interp r fds))]
   [multC (l r) (* (interp l fds) (interp r fds))]
   [factC (x) (cond
              [(= x 1) 1]
              [else (* x (interp (factC (- x 1)) fds))])]
   [ifC (c y n) (cond
          [(> (interp c fds) 0) (interp y fds)]
          [else (interp n fds)])]))

;subst working with interpreter it put variables.
;subst: what:ExprC,for:symbol,in:ExprC ---> ExprC
(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
 (type-case ExprC in
 [numC (n) in]
 [idC (s) (cond
            [(symbol=? s for) what]
            [else in])]
 [appC (f a) (appC f (subst what for a))]
 [plusC (l r) (plusC (subst what for l)
                     (subst what for r))]
 [multC (l r) (multC (subst what for l)
                     (subst what for r))]
 [factC (l) (factC (subst what for l)
                     )]  
 [ifC (c y n) (ifC (subst what for c)
                     (subst what for y)
                     (subst what for n))]))


;Contract:
;expression --> ExprC
;parsing expressions
(define (parse [s : s-expression]) : ExprC
 (cond
   [(s-exp-number? s)(numC (s-exp->number s))]
   [(s-exp-symbol? s) (idC (s-exp->symbol s))]
   [(s-exp-list? s)
    (let ([sl (s-exp->list s)])
      (cond
        [(= (length sl) 2)
         [(appC (s-exp->symbol (second sl)) (parse (third sl)))]]
     [(= (length sl) 3)
      (case (s-exp->symbol (first sl))
        [(+) (plusC (parse (second sl)) (parse (third sl)))]
        [(*) (multC (parse (second sl)) (parse (third sl)))]         
        [else ((error 'parse "invalid list input"))])]
       [else (error 'parse "invalid input")]))]
   [else (error 'parse "invalid input")]))

;TEST
(test (parse '(+ 2 3)) (plusC (numC 2)(numC 3)))
(test (parse '(* 2 3)) (multC (numC 2)(numC 3)))




(define fx (list 
                   [fdC 'double 'x (parse '(+ x x))]
                   [fdC 'sqr 'y (parse '(* y y))]
   ))



;λ calculus GRAMMAR:
;;that defines above
;;λ-sym: symbol
;;λ-app l:λ-exp,r:λ-exp
;;λdef symbol,λ-exp
(define-type λ-exp
 (λ-sym (v : symbol))
 (λ-app (l : λ-exp)(r : λ-exp))
 (λ-def (v : symbol)(p : λ-exp))
 )

;Contract:
;lambdaparser: sexp:s-expression ---> λ-exp
;parsing lambda calculus expressions
;;transform given lambda expressions to expressions
(define (lambdaparser (sexp : s-expression)) : λ-exp
 (cond
   [(s-exp-symbol? sexp)(λ-sym (s-exp->symbol sexp))]
   [(s-exp-list? sexp)
    (let ([sexp-list (s-exp->list sexp)])
      (cond
        [(= 2 (length sexp-list))
         (λ-app (lambdaparser (first sexp-list))(lambdaparser (second sexp-list)))]
        [(= 3 (length sexp-list))
         (if (and (symbol=? 'λ (s-exp->symbol (first sexp-list)))
                  (s-exp-symbol? (second sexp-list)))
             (λ-def (s-exp->symbol(second sexp-list))
                    (lambdaparser (third sexp-list)))
             (error 'lambdaparser "Not valid λ-def")
             )]
        [else (error 'lambdaparser "Not valid length λ-exp")]
        ))]
   [else (error 'parser "Not valid λ-exp")]
))

(test (lambdaparser (symbol->s-exp 'y))(λ-sym 'y))
(test (lambdaparser '(λ x x))(λ-def 'x (λ-sym 'x)))
(test (lambdaparser '((λ x x) y))(λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))
(test (lambdaparser '((λ x x)(λ y y)))
      (λ-app (λ-def 'x (λ-sym 'x))(λ-def 'y (λ-sym 'y))))
(test (lambdaparser '(λ x (λ y (y x))))
      (λ-def 'x (λ-def 'y (λ-app (λ-sym 'y) (λ-sym 'x)))))


;lambda substition
;Contract:
;λ-exp,symbol,λ-exp ---> λ-exp
;; Template:
;; (define 
;; (substituter [what : λ-exp] [for : symbol] [in : λ-exp]) : λ-exp
;; body
(define (lambdasubst [what : λ-exp] [for : symbol] [in : λ-exp]) : λ-exp 
 (type-case λ-exp in
   (λ-sym (v) (if (symbol=? v for) 
                  what
                  in))
   (λ-app (l r) (λ-app (lambdasubst what for l)
                       (lambdasubst what for r)))
   (λ-def (v p)(λ-def v (lambdasubst what for p)))
   )
 )

;Contract
;listofsymbol,listfofsymbol ---> listof symbol
(define (union (s1 : (listof symbol)) (s2 : (listof symbol))) : (listof symbol)
 (foldr (lambda (x y)
          (if (member x y)
              y
              (cons x y))) 
        empty
        (append s1 s2)))

;; Tests:
(test (union empty empty) empty)
(test (union empty (list 'x)) (list 'x))
(test (union (list 'x)(list 'x 'y)) (list 'x 'y))


;;set-difference:listof symbol,listof symbol -> listof symbol
;;To find the set difference of two sets.
(define (difference (s1 : (listof symbol))  (s2 : (listof symbol))) : (listof symbol)
 (filter (lambda (x)
           (not (member x s2)))
         s1))

;; Tests:
(test (difference empty (list 'x)) empty)
(test (difference (list 'x) empty) (list 'x))
(test (difference (list 'x)(list 'x 'y)) empty)
(test (difference (list 'x 'y)(list 'x))(list 'y))

;Contract
;λ-exp ---->listof symbol
;;lambda calculus identifier
;;Template
;;(define(identifier (λ-exp)): (listof symbol)
;;(type-case λ-exp le
;; body
(define (identifier (le : λ-exp)) : (listof symbol)
 (type-case λ-exp le
   (λ-sym (v) (list v))
   (λ-app (l r)(set-union 
                (identifier l)
                (identifier r)))
   (λ-def (v p)(difference (identifier p)
                               (list v)))
   ))

;;TESTS

(test (identifier (lambdaparser (symbol->s-exp 'x)))(list 'x))
(test (identifier (lambdaparser '(λ x y))) (list 'y))
(test (identifier (lambdaparser '((λ y x) (λ y x)))) (list 'x))
(test (identifier (lambdaparser '(λ x (λ y (y x))))) empty)
