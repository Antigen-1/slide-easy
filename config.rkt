#lang slideshow
(provide (contract-out (current-slide-constructor (parameter/c (-> pict? any/c)))
                       (current-pict-combinator (parameter/c (-> (listof pict?) pict?))))
         (all-from-out slideshow))
(define current-slide-constructor (make-parameter slide))
(define current-pict-combinator (make-parameter (lambda (pl) (apply vl-append (current-gap-size) pl))))
