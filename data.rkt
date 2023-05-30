#lang racket/base
(require racket/contract pict)
(provide install ->pict construct)

;;--------------------------
;;the vertical barrier and generic interfaces
(define table (make-hasheq))

(define (tag? o) (and (symbol? o) (symbol-interned? o)))
(define (already-has? t) (hash-has-key? table t))

(define/contract (install type contract ->pict (construct values)) ;;install a new datatype, which requires a type tag, a contract, a `->pict` function, and an optional constructor
  (->i ((type (and/c tag? (not/c already-has?))) (contract contract?) (->pict (contract) (-> contract pict?))) ((construct (contract) (-> any/c ... contract))) (result any/c))
  (hash-set! table type (vector ->pict construct)))
(define/contract (index type op)
  (-> (and/c tag? already-has?) (or/c 'construct '->pict) any/c)
  (vector-ref (hash-ref table type)
              (case op
                ((construct) 1)
                ((->pict) 0))))

(struct tagged-object (tag content))

(define/contract (->pict obj)
  (-> tagged-object? pict?)
  (define tag (tagged-object-tag obj))
  ((index tag '->pict) (tagged-object-content obj)))
(define/contract (construct type . args)
  (-> (and/c tag? already-has?) any/c ... tagged-object?)
  (tagged-object type (apply (index type 'construct) args)))
;;--------------------------

(module+ test
  (require sugar/debug rackunit slideshow/base)

  (test-case
      "data"
    (install 'title string? titlet)

    (define (process s) (->pict (construct 'title s)))
    (define (process1 s) (titlet s))
    
    (compare (time-repeat 100 (process "Hello, World!"))
             process
             process1)))
