#lang racket/base
(require "config.rkt" "data.rkt" racket/function slideshow/base pict racket/contract
         (for-syntax racket/base))
(provide config ~p (rename-out (n:install install) (#%slide-app #%app) (#%app #%call)))

(define-syntax-rule (n:install infm ...)
  (begin (install . infm) ...))

(define-syntax (~p stx)
  (syntax-case stx ()
    ((_ cont)
     #'(->pict cont))
    ((_ type cont)
     #'(->pict (tag type cont)))))

(define-syntax (config stx)
  (syntax-case stx ()
    ((_ (token ...)) #'(current-slide-configure (curry slide token ...)))
    ((_ (token ...) body ...)
     #'(parameterize ((current-slide-configure (curry slide token ...)))
         body ...))))

(define-syntax-rule (#%slide-app . tokens)
  (let ()
    (define/contract result pict? (#%app . tokens))
    ((current-slide-configure) result)
    result))
