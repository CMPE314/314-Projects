#lang plai-typed

;;FURKAN ÇELİK
;;111200035

;; lambda-expression grammar
;; lambda -> v
;; lambda -> (lambda lambda)
;; lambda -> (lambda v lambda)
;; lambda-exp is grammar 

(define-type λ-exp
  (λ-sym (v : symbol))
  (λ-app (l : λ-exp)(r : λ-exp))
  (λ-def (v : symbol)(p : λ-exp))
  )

;Substitution
;Purpose
;Method will substitute λ-exp with other expression in λ-exp according to cond
;λ-exp-> λ-exp
;Template
; (define (substitution [what : λ-exp] [for : symbol] [in : λ-exp]) : λ-exp
;  (type-case λ-exp in
;    (λ-sym (v) ...)
;    (λ-app (l r) ...)
;    (λ-def (v p) ...))

(define (substitution [what : λ-exp] [for : symbol] [in : λ-exp]) : λ-exp 
  (type-case λ-exp in
    (λ-sym (v) (if (symbol=? v for) 
                   what
                   in))
    (λ-app (l r) (λ-app (substitution what for l)
                        (substitution what for r)))
    (λ-def (v p)(λ-def v (substitution what for p)))
    )
  )



;; betatrans : λ-exp -> λ-exp
;; Purpose :Beta-transformation and reduction implementation of λ-calculus.
;; Template :
; (define (betatrans (le : λ-exp)) : λ-exp
;  (type-case λ-exp le
;    (λ-sym (v) ...)
;    (λ-app (l r) .....)
;    (λ-def (v p) .....)))
(define (betatrans (le : λ-exp)) : λ-exp
  (type-case λ-exp le
    (λ-sym (v) le) 
    (λ-app (l r) (if (λ-def? l)
                     (substitution r (λ-def-v l) (λ-def-p l))
                     (λ-app (betatrans l) (betatrans r))))
    (λ-def (v p) (λ-def v (betatrans p)))))



;; parser s-expression ->  λ-exp
;Purpose
;; Convert s expression into the λ-exp form
;Template
;(define (parser [s : s-expression]) :  λ-exp
;     (cond
;     [(s-exp-symbol? s) (λ-sym (s-exp->symbol s))]
;          [(s-exp-list? s)
;          (let ([sl (s-exp->list s)])
;          (case (s-exp->symbol (first sl))
;          [else ....]
;          [else ....)

(define (parser (se : s-expression)) : λ-exp
  (cond
    [(s-exp-symbol? se)(λ-sym (s-exp->symbol se))]
    [(s-exp-list? se)
     (let ([se-list (s-exp->list se)])
       (cond
         [(= 2 (length se-list))
          (λ-app (parser (first se-list))(parser (second se-list)))]
         [(= 3 (length se-list))
          (if (and (symbol=? 'λ (s-exp->symbol (first se-list)))
                   (s-exp-symbol? (second se-list)))
              (λ-def (s-exp->symbol(second se-list))
                     (parser (third se-list)))
              (error 'parser "error")
              )]
         [else (error 'parser "error")]
         ))]
    [else (error 'parser "error")]
))


;;Examples
(define lambdasq
  (parser '(λ g (λ x (g (g x))))))

(define lambdatrip
  (parser '(λ g (λ x (g (g (g x)))))))

;;combo--> combination lambdasq and lambdatrip
(define combo 
  (λ-app lambdasq lambdatrip))





;;Tests for lambdasq

(test(betatrans lambdasq)(λ-def 'g (λ-def 'x (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-sym 'x))))))

(test(betatrans(betatrans lambdasq))(λ-def 'g (λ-def 'x (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-sym 'x))))))

(test(betatrans(betatrans(betatrans lambdasq)))(λ-def 'g (λ-def 'x (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-sym 'x))))))

(test(betatrans(betatrans(betatrans(betatrans lambdasq))))(λ-def 'g (λ-def 'x (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-sym 'x))))))

(test(betatrans(betatrans(betatrans(betatrans(betatrans lambdasq)))))(λ-def 'g (λ-def 'x (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-sym 'x))))))



;;test for lambdatrip
(test (betatrans lambdatrip)(λ-def 'g (λ-def 'x (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-sym 'x)))))))

(test (betatrans(betatrans lambdatrip))(λ-def 'g (λ-def 'x (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-sym 'x)))))))

(test(betatrans(betatrans(betatrans lambdatrip)))(λ-def 'g (λ-def 'x (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-sym 'x)))))))

(test(betatrans(betatrans(betatrans(betatrans lambdatrip))))(λ-def 'g (λ-def 'x (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-sym 'x)))))))



;;test for combo
(test(betatrans(betatrans combo))(λ-def
 'x
 (λ-def
  'x
  (λ-app
   (λ-app (λ-def 'g (λ-def 'x (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-sym 'x)))))) (λ-sym 'x))
   (λ-app
    (λ-app (λ-def 'g (λ-def 'x (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-sym 'x)))))) (λ-sym 'x))
    (λ-app
     (λ-app (λ-def 'g (λ-def 'x (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-app (λ-sym 'g) (λ-sym 'x)))))) (λ-sym 'x))
     (λ-sym 'x)))))))
