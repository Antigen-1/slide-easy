#lang racket/base
(require "../generic.rkt" racket/string racket/list rackunit sugar/debug)

(define type0 (make-type 'text))
(define type1 (make-type 'list type0))
(check-true (has-prefix? type1 type0))

(define (string-list-length l) (foldl (lambda (s i) (+ i (string-length s))) 0 l))

(install type0 string? text (cons 'len string-length) (cons 'val values))
(install type1 (listof string?) string-append* (cons 'len string-list-length))
(check-exn exn:fail:contract? (lambda () (install type0 string? text)))

(define (len o) (apply-generic 'len o))
(define (val o) (apply-generic 'val o))

(define strlst (make-list (random 10 100) (make-string (random 100 200) #\a)))

(define obj1 (tag type0 (string-append* strlst)))
(define obj2 (coerce (tag type1 strlst) 'text))

(check-equal? (val obj1) (val obj2))
(check-true (= (string-length (string-append* strlst)) (len obj1) (len obj2)))

(define (process s) (text (string-append* s)))
(define (process1 s) (->pict (tag type0 (string-append* s))))
(define (process2 s) (->pict (tag type1 s)))
(define (process3 s) (->pict (coerce (tag type1 strlst) 'text)))

(call-with-values (lambda () (compare (time-repeat 1000 (process strlst)) process process1 process2 process3)) void)
