#lang racket/base
(require "config.rkt" "data.rkt" racket/function slideshow/base pict racket/contract
         (for-syntax racket/base))
(provide config ~o (rename-out (n:install install) (#%slide-app #%app) (#%app #%ret)))

(define-syntax-rule (n:install infm ...)
  (begin (install . infm) ...))

(define-syntax (~o stx)
  (syntax-case stx ()
    ((_ obj)
     #'(->pict obj))
    ((_ type cont ...)
     #'(construct type cont ...))))

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
