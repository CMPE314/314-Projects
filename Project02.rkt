#lang plai-typed


;msl is a language
(define-type msl
  [msl-num (n : number)]
  [msl-add (lhs : msl) (rhs : msl)]
  [msl-mul (lhs : msl) (rhs : msl)]
  [msl-sub (lhs : msl) (rhs : msl)]
  )

;eval is evaluator, calculator.
;evaluate a msl expression
(define (eval [expr : msl])
  (type-case msl expr
    [msl-num (n) n]
    [msl-add (lhs rhs) (+ (eval lhs) (eval rhs))]
    [msl-sub (lhs rhs) (+ (eval lhs) (eval rhs))]
    [msl-mul (lhs rhs) (* (eval lhs) (eval rhs))]
    ))

;;tests
(test (eval (msl-num 7))  7)
(test (eval (msl-add (msl-num 3) (msl-num 4)))  7)
(test (eval (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))  42)
(test (eval (msl-mul (msl-num 0) (msl-num 4)))  0)
(test (eval (msl-sub (msl-num 7) (msl-num 4)))  3)
(test (eval (msl-add (msl-num 32) (msl-num 4)))  3)
(test (eval (msl-num 3))  7)
(test (eval (msl-mul (msl-num 3) (msl-num 4)))  12)


          


;parse s expression -> msl
;convert s expression to msl form
(define (parse [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (msl-add (parse (second sl)) (parse (third sl)))]
         [(*) (msl-mul (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(test (parse '7) (msl-num 7))
(test (parse '(+ 3 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse '(+ (+ 3 4) 35)) (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))



;Defining ArithS
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [uminusS (l : ArithS)])





(define (desugar [as : ArithS]) : msl
  (type-case ArithS as
    [numS (n) (msl-num n)]
    [plusS (l r) (msl-add (desugar l)
                        (desugar r))]
    [multS (l r) (msl-mul (desugar l)
                        (desugar r))]
    [bminusS (l r) (msl-add (desugar l)
                      (msl-mul (msl-num -1) (desugar r)))]
    [uminusS (l) (desugar (bminusS (numS -1) l))]))



;;tests for Desuggaring

(test (desugar (numS 7 )) (msl-num 7))
(test (desugar (numS 0 )) (msl-num 0))
(test (desugar (numS -5 )) (msl-num -5))
(test (desugar (numS -1 )) (msl-num -1))
(test (desugar (numS 23 )) (msl-num 23))


;(test (desugar (plusS (numS 7 ) (numS 4))) (msl-num 11))
;(test (desugar (plusS (numS 5 ) (numS 4 ))) (msl-num 15))
;(test (desugar (plusS (numS 1 ) (numS 4 ))) (msl-num 5))
;(test (desugar (plusS (numS 51 ) (numS 43 ))) (msl-num 94))
(test (desugar (plusS (numS 10 ) (numS 0 ))) (msl-num 10))


;(test (desugar (multS (numS 5 ) (numS 4 ))) (msl-num 20))
;(test (desugar (multS (numS 6 ) (numS 6 ))) (msl-num 36))
;(test (desugar (multS (numS -5 ) (numS 1 ))) (msl-num -5))
;(test (desugar (multS (numS 11 ) (numS 6 ))) (msl-num 66))
(test (desugar (multS (numS 2 ) (numS 4 ))) (msl-num 8))

;(test (desugar (bminusS (numS 5 ) (numS 4 ))) (msl-num 1))
;(test (desugar (bminusS (numS 8 ) (numS 4 ))) (msl-num 4))
;(test (desugar (bminusS (numS 4 ) (numS 4 ))) (msl-num 0))
;(test (desugar (bminusS (numS 3 ) (numS 4 ))) (msl-num -1))
(test (desugar (bminusS (numS 0 ) (numS 0 ))) (msl-num 0))

;(test (desugar (uminusS (numS 5 )))  (msl-num -5))
;(test (desugar (uminusS (numS 7 ))) (msl-num -7))
;(test (desugar (uminusS (numS 0 )))  (msl-num 0))
;(test (desugar (uminusS (numS 1 )))  (msl-num -1))
(test (desugar (uminusS (numS -5 )))  (msl-num -5))



(define (unparser-reverse-polish [expr : msl])
  (type-case msl expr
    (msl-num (n) (list (number->s-exp n)))
    (msl-add (lhs rhs) (append (append (unparser-reverse-polish lhs) (unparser-reverse-polish rhs))(list (symbol->s-exp '+))))
    (msl-sub (lhs rhs) (append  (append (unparser-reverse-polish lhs) (unparser-reverse-polish rhs)) (list(symbol->s-exp '-))))
    (msl-mul (lhs rhs) (append (append (unparser-reverse-polish lhs) (unparser-reverse-polish rhs))(list (symbol->s-exp '*))))))













