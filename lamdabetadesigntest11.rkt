
#lang plai-typed
;;FURKAN ÇELİK
;;111200035
;; lambda-expression grammar
;; lambda -> v
;; lambda -> (lambda lambda)
;; lambda -> (lambda v lambda)
;; lambda-exp is grammar
;;Code
;;



;Substitution
;Purpose
;Method will substitute λ-exp with other expression in λ-exp according to cond
;λ-exp-> λ-exp
;Template
; (define (substitution [what : λ-exp] [for : symbol] [in : λ-exp]) : λ-exp
; (type-case λ-exp in
; (λ-sym (v) ...)
; (λ-app (l r) ...)
; (λ-def (v p) ...))
;;Code
;;






;; betatrans : λ-exp -> λ-exp
;; Purpose :Beta-transformation and reduction implementation of λ-calculus.
;; Template :
; (define (betatrans (le : λ-exp)) : λ-exp
; (type-case λ-exp le
; (λ-sym (v) ...)
; (λ-app (l r) .....)
; (λ-def (v p) .....)))
;;Code
;;





;; parser s-expression -> λ-exp
;Purpose
;; Convert s expression into the λ-exp form
;Template
;(define (parser [s : s-expression]) : λ-exp
; (cond
; [(s-exp-symbol? s) (λ-sym (s-exp->symbol s))]
; [(s-exp-list? s)
; (let ([sl (s-exp->list s)])
; (case (s-exp->symbol (first sl))
; [else ....]
; [else ....)
;;Code
;;




;;Examples

;;Tests for lambdasq
;;Test codes
