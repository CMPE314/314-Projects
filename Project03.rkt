#lang plai-typed


;Grammar
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifC (c : ExprC) (s : ExprC) (n : ExprC)])


;Function Defination
(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])




;help to find function name , definitions , identifier
;list of fundefC —> fundefC
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))




; Evaluate expressions to numbers
(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (_) (error 'interp "shouldn't get here")]
    [appC (f arg) (let [(fd (get-fundef f fds))]
                    (interp (subst (numC (interp arg fds))
                                   (fdC-arg fd)
                                   (fdC-body fd))
                            fds))]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    [ifC (c y n) (cond
           [(> (interp c fds) 0) (interp y fds)]
           [else (interp n fds)])]))


;Substitute expressions in ExprC according to conditions.
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
  [ifC (c y n) (ifC (subst what for c)
                      (subst what for y)
                      (subst what for n))]))   


(define trylist (fdC 'double 'x (plusC (idC 'x) (idC 'x))))

;;test for interp numC
(test (interp (numC 7) (list trylist)) 7)
(test (interp (numC -3) (list trylist)) -3)
(test (interp (numC 7) (list trylist)) 0)
(test (interp (numC 0) (list trylist)) 0)
(test (interp (numC 17) (list trylist)) 17)

;;test for interp appC

(test (interp (appC 'double (numC 7)) (list trylist)) 14)
(test (interp (appC 'double (numC -5)) (list trylist)) -10)
(test (interp (appC 'double (numC 2)) (list trylist)) 8)
(test (interp (appC 'double (numC 0)) (list trylist)) 0)
(test (interp (appC 'quadruple (numC 1)) (list trylist)) 4)

;;test for plusC 
(test(interp (plusC (numC 4) (numC 6)) (list trylist)) 10)
(test(interp (plusC (numC -5) (numC 5)) (list trylist)) 0)
(test(interp (plusC (numC 1) (numC 2)) (list trylist)) 3)
(test(interp (plusC (numC 110) (numC 6)) (list trylist)) 116)
(test(interp (plusC (numC 0) (numC -5)) (list trylist)) -5)


;;test for multC
(test(interp (multC (numC 4) (numC 6)) (list trylist)) 24)
(test(interp (multC (numC 0) (numC -34234)) (list trylist)) 0)
(test(interp (multC (numC 1) (numC 43)) (list trylist)) 43)
(test(interp (multC (numC -5) (numC 3)) (list trylist)) -15)
(test(interp (multC (numC 1) (numC 1)) (list trylist)) 1)

;;greater-than-zero primitive
(test (interp (ifC (numC 7) (numC 1) (numC 0)) (list trylist)) 1)
(test (interp (ifC (numC -20) (numC 1) (numC 0)) (list trylist)) 0)
(test (interp (ifC (numC 0) (numC 1) (numC 0)) (list trylist)) 0)
(test (interp (ifC (numC 1) (numC 1) (numC 0)) (list trylist)) 1)
(test (interp (ifC (plusC  (numC 5) (numC -6)) (numC 1) (numC 0)) (list trylist)) 0)
(test (interp (ifC (multC (numC -1) (numC -2)) (numC 1) (numC 0)) (list trylist)) 1)
(test (interp (ifC (appC 'double (numC 7)) (numC 1) (numC 0)) (list trylist)) 1)
(test (interp (ifC (plusC (numC 1) (numC 2)) (numC 1) (numC 0)) (list trylist)) 1)
(test (interp (ifC (multC (numC -5) (numC 3)) (numC 1) (numC 0)) (list trylist)) 0)
(test (interp (ifC (appC 'double (numC 0)) (numC 1) (numC 0)) (list trylist)) 0)







