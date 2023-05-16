#lang racket/base
(require pict racket/contract)
(provide (contract-out (current-init-pict (parameter/c pict?))
                       (left-to-right? (parameter/c boolean?)))
         (all-from-out pict racket/base))

(define current-init-pict (make-parameter (blank)))
(define left-to-right? (make-parameter #f))
