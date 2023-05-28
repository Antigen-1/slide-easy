#lang racket/base
(require slideshow/base)
(provide current-slide-configure (all-from-out slideshow/base))

(define current-slide-configure (make-parameter slide))
