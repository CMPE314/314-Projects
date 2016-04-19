#lang plai-typed


(define (parse [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s)(numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (cond 
                [(equal? (length sl) 3) (bminusS (parse (second sl)) (parse (third sl)))]
                [(equal? (length sl) 2) (uminusS (parse (second sl)))]
                [else (error 'parse "invalid minus")])]
         [(function) (appS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else ((error 'parse "invalid list input"))]))]
    [else (error 'parse "invalid input")]))




(test (parse '7) (numS 7))
(test (parse '3) (numS 3))
(test (parse '6) (numS 6))
(test (parse '23) (numS 23))
(test (parse '0) (numS 0))

(test (parse '(+ 3 4)) (plusS (numS 3) (numS 4)))
(test (parse '(+ -3 24)) (plusS (numS -3) (numS 24)))
(test (parse '(+ 1 0)) (plusS (numS 1) (numS 0)))
(test (parse '(+ 0 -3)) (plusS (numS 0) (numS -3)))
(test (parse '(+ 11 -11)) (plusS (numS 11) (numS -11)))

(test (parse '(* 0 -11)) (multS (numS 0) (numS -11)))
(test (parse '(* 1 222342)) (multS (numS 1) (numS 222342)))
(test (parse '(* 2 3)) (multS (numS 2) (numS 3)))
(test (parse '(* 4 -5)) (multS (numS 4) (numS -5)))
(test (parse '(* 5 5)) (multS (numS 5) (numS -5)))


(test (parse '(function double 3))(appS 'double (numS 3)))
(test (parse '(function double 6))(appS 'double (numS 6)))
(test (parse '(function double 0))(appS 'double (numS 0)))
(test (parse '(function double -5))(appS 'double (numS -5)))
(test (parse '(function double 1))(appS 'double (numS 1)))

(test (parse '(function quadruple 3))(appS 'quadruple (numS 3)))
(test (parse '(function quadruple 2))(appS 'quadruple (numS 2)))
(test (parse '(function quadruple 4))(appS 'quadruple (numS 4)))
(test (parse '(function quadruple -3))(appS 'quadruple (numS -3)))
(test (parse '(function quadruple 0))(appS 'quadruple (numS 0)))

(test(parse '(+ x x))(plusS (idS 'x) (idS 'x)))
(test(parse '(* x x))(multS (idS 'x) (idS 'x)))
(test(parse '(- x x))(bminusS (idS 'x) (idS 'x)))
(test(parse '(+ 2x x))(plusS (idS '2x) (idS 'x)))
(test(parse '(+ x -x))(plusS (idS 'x) (idS '-x)))

(test (parse '(function sqr 4))(appS 'double (numS 3)))
(test (parse '(function sqr 8))(appS 'double (numS 2)))
(test (parse '(function sqr 1))(appS 'double (numS 4)))
(test (parse '(function sqr -1))(appS 'double (numS -3)))
(test (parse '(function sqr 0))(appS 'double (numS 0)))




(define fnc (list 
                    [fdC 'double 'x (desugar(parse '(+ x x)))]
                    [fdC 'quadruple 'x (desugar(parse '(* 4 x)))]
                    [fdC 'sqr 'y (desugar(parse '(* y y)))]
                    [fdC 'new 'z (desugar(parse '(+ x (+ y y))))]))


;;test for interp numC
(test (interp (numC 7)    fnc) 7)
(test (interp (numC -3)    fnc) -3)
(test (interp (numC 7)    fnc) 7)
(test (interp (numC 0)    fnc) 0)
(test (interp (numC 17)    fnc) 17)

;;test for interp appC

(test (interp (appC 'double (numC 7))    fnc) 14)
(test (interp (appC 'double (numC -5))   fnc) -10)
(test (interp (appC 'double (numC 2))    fnc) 4)
(test (interp (appC 'double (numC 0))    fnc) 0)
(test (interp (appC 'quadruple (numC 1))    fnc) 4)

;;test for plusC 
(test(interp (plusC (numC 4) (numC 6))   fnc) 10)
(test(interp (plusC (numC -5) (numC 5))    fnc) 0)
(test(interp (plusC (numC 1) (numC 2))    fnc) 3)
(test(interp (plusC (numC 110) (numC 6))    fnc) 116)
(test(interp (plusC (numC 0) (numC -5))    fnc) -5)


;;test for multC
(test(interp (multC (numC 4) (numC 6))    fnc) 24)
(test(interp (multC (numC 0) (numC -34234))    fnc) 0)
(test(interp (multC (numC 1) (numC 43))    fnc) 43)
(test(interp (multC (numC -5) (numC 3))  fnc) -15)
(test(interp (multC (numC 1) (numC 1))  fnc) 1)



;;tests for if greater-than-zero primitive
(test (interp (ifC (numC 7) (numC 1) (numC 0))   fnc) 1)
(test (interp (ifC (numC -20) (numC 1) (numC 0))    fnc) 0)
(test (interp (ifC (numC 0) (numC 1) (numC 0))    fnc) 0)
(test (interp (ifC (numC 1) (numC 1) (numC 0))    fnc) 1)
(test (interp (ifC (plusC  (numC 5) (numC -6)) (numC 1) (numC 0))   fnc) 0)
(test (interp (ifC (multC (numC -1) (numC -2)) (numC 1) (numC 0))   fnc) 1)
(test (interp (ifC (appC 'double (numC 7)) (numC 1) (numC 0))    fnc) 1)
(test (interp (ifC (plusC (numC 1) (numC 2)) (numC 1) (numC 0))  fnc) 1)
(test (interp (ifC (multC (numC -5) (numC 3)) (numC 1) (numC 0))  fnc) 0)
(test (interp (ifC (appC 'double (numC 0)) (numC 1) (numC 0))  fnc) 0)




;;tests
;;interp-->desugar-->parse
;;(test (interp (desugar (parse(...))
(test (interp (desugar (parse '7))  fnc) 7)
(test (interp (desugar (parse '3))  fnc) 3)
(test (interp (desugar (parse '6))  fnc) 6)
(test (interp (desugar (parse '(+ 3 4)))  fnc) 7)
(test (interp (desugar (parse '(+ 4 46)))  fnc) 50)

(test (interp (desugar (parse '(* 1 222342)))  fnc) 222342)
(test (interp (desugar (parse '(* 4 -5)))  fnc) -20)
(test (interp (desugar (parse '(- 11 -11)))  fnc) 22)
(test (interp (desugar (parse '(* 5 5)))  fnc) 25)
(test (interp (desugar (parse '(+ 2 4)))  fnc) 6)


(test (interp (desugar (parse '(function double 3)))  fnc) 6)
(test (interp (desugar (parse '(function double 6)))  fnc) 12)
(test (interp (desugar (parse '(function double 0)))  fnc) 0)
(test (interp (desugar (parse '(function double -5)))  fnc) -10)
(test (interp (desugar (parse '(function double 1)))  fnc) 2)

(test (interp (desugar (parse '(function quadruple 3)))  fnc) 12)
(test (interp (desugar (parse '(function quadruple 2)))  fnc) 8)
(test (interp (desugar (parse '(function quadruple 4)))  fnc) 16)
(test (interp (desugar (parse '(function quadruple -3)))  fnc) -12)
(test (interp (desugar (parse '(function quadruple 0)))  fnc) 0)


(test (interp (desugar (parse '(function sqr 4)))  fnc) 16)
(test (interp (desugar (parse '(function sqr 8)))  fnc) 64)
(test (interp (desugar (parse '(function sqr 1)))  fnc) 1)
(test (interp (desugar (parse '(function sqr -1)))  fnc) 1)
(test (interp (desugar (parse '(function sqr 0)))  fnc) 0)

      

