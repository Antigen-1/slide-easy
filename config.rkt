#lang racket/base
(require slideshow/base)
(provide current-slide-configure)

(define current-slide-configure (make-parameter slide))
