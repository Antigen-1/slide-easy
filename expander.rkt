#lang racket/base
(require "config.rkt" "data.rkt")
(provide (rename-out (#%slide-app #%app)) #%call)

(define-syntax-rule (#%slide-app . tokens)
  (let ((result (->pict (#%app . tokens))))
    ((current-slide-configure) result)
    result))

(define-syntax-rule (#%call . tokens)
  (#%app . tokens))
