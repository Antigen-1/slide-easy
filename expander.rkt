#lang racket/base
(require "config.rkt" (prefix-in o: "data.rkt") racket/function slideshow/base)
(provide install tag config (rename-out (#%slide-app #%app)) #%call)

(define-syntax-rule (install type pred ->pict)
  (o:install type pred ->pict))

(define-syntax-rule (tag tag content)
  (o:tag tag content))

(define-syntax-rule (config token ...)
  (current-slide-configure (curry slide token ...)))

(define-syntax-rule (#%slide-app . tokens)
  (let ((result (->pict (#%app . tokens))))
    ((current-slide-configure) result)
    result))

(define-syntax-rule (#%call . tokens)
  (#%app . tokens))
