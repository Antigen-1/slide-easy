#lang racket/base
(require "tokenizer.rkt" "parser.rkt" br/syntax racket/syntax)
(provide read-syntax)

(define (read-syntax path port)
  (strip-bindings
   #`(module #,(generate-temporary 'slide-easy) slide-easy/expander
       #,(parse path (make-tokenizer port path)))))
