#lang racket/base
(require "config.rkt" racket/function slideshow/base "data.rkt"
         (for-syntax racket/base))
(provide config (rename-out (#%slide-app #%app) (#%app #%ret)))

(define-syntax (config stx)
  (syntax-case stx ()
    ((_ (token ...)) #'(current-slide-configure (curry slide token ...)))
    ((_ (token ...) body ...)
     #'(parameterize ((current-slide-configure (curry slide token ...)))
         body ...))))

(define-syntax-rule (#%slide-app . tokens)
  (let ((result (#%app . tokens)))
    ((current-slide-configure) (->pict result))
    result))
