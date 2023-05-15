#lang racket/base
(require pict racket/contract)
(provide (contract-out (ghost-when-reslide? (parameter/c boolean?)))
         (all-from-out pict racket/base))
(define ghost-when-reslide? (make-parameter #t))
