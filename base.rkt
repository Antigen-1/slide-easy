#lang racket/base
(require "config.rkt" racket/function "generic.rkt" (for-syntax racket/base))
(provide config (rename-out (#%slide-app #%app) (#%app #%ret)))

(define-syntax (config stx)
  (syntax-case stx ()
    ((_ (token ...)) #'(current-slide-configure (curry slide token ...)))
    ((_ (token ...) body ...)
     #'(parameterize ((current-slide-configure (curry slide token ...)))
         body ...))))

(define-syntax-rule (#%slide-app . tokens)
  (call-slide (#%app . tokens)))

(define (call-slide obj) ((current-slide-configure) (->pict obj)) obj)
