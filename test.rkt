#lang racket/base
(module+ test
  (require (submod "main.rkt" generic) racket/string racket/list rackunit sugar/debug)

  (define type0 (make-type 'text))
  (define type1 (make-type 'list type0))

  (define (string-list-length l) (foldl (lambda (s i) (+ i (string-length s))) 0 l))
  
  (install type0 string? text (cons 'len string-length))
  (install type1 (listof string?) string-append* (cons 'len string-list-length))

  (define (len o)
    (apply-generic 'len o))

  (define strlst (make-list (random 10 100) (make-string (random 100 200) #\a)))
  
  (test-case
      "with contract"
    (check-true (= (string-length (string-append* strlst)) (len (tag type0 (string-append* strlst))) (len (coerce (tag type1 strlst) 'text))))

    (define (process s) (text (string-append* s)))
    (define (process1 s) (->pict (tag type0 (string-append* s))))
    (define (process2 s) (->pict (tag type1 s)))
    (define (process3 s) (->pict (coerce (tag type1 strlst) 'text)))
    
    (compare (time-repeat 1000 (process strlst))
             process process1 process2 process3)))
