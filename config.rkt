#lang slideshow
(provide (contract-out (current-slide-constructor (parameter/c (-> pict? ... any/c)))) (all-from-out slideshow))
(define current-slide-constructor (make-parameter slide))
