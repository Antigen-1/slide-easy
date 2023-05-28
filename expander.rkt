#lang racket/base
(require slideshow/base)
(provide (rename-out (#%slide-app #%app)) #%call)

(define-syntax-rule (#%slide-app . tokens)
  (let ((result (#%app . tokens)))
    (slide result)
    result))

(define-syntax-rule (#%call . tokens)
  (#%app . tokens))
