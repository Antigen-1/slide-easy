#lang racket/base
(require "config.rkt" (prefix-in o: "data.rkt") racket/function slideshow/base racket/contract)
(provide install tag config ->pict (rename-out (#%slide-app #%app)) #%call)

(define-syntax-rule (install type pred ->pict)
  (o:install type pred ->pict))

(define-syntax-rule (tag tag content)
  (o:tag tag content))

(define-syntax-rule (config token ...)
  (current-slide-configure (curry slide token ...)))

(define-syntax-rule (->pict obj)
  (o:->pict obj))

(define-syntax-rule (#%slide-app . tokens)
  (let ()
    (define/contract result pict? (#%app . tokens))
    ((current-slide-configure) result)
    result))

(define-syntax-rule (#%call . tokens)
  (#%app . tokens))
