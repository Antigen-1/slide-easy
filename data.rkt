#lang racket/base
(require racket/contract pict)
(provide install apply-generic ->pict (rename-out (tagged-object tag) (tagged-object-content content)))

;;--------------------------
;;the vertical barrier and generic interfaces
(define table (make-hasheq))

(define (tag? o) (and (symbol? o) (symbol-interned? o)))
(define (has-key? p) (hash-has-key? table p))

(define/contract (install type contract ->pict . rest) ;;install a new datatype
  (->i ((type (and/c tag? (not/c has-key?))) (contract contract?) (->pict (contract) (-> contract pict?)))
       #:rest (rest (contract) (listof (cons/c (and/c tag? (not/c '->pict)) (-> contract any/c ... any))))
       any)
  (hash-set! table type (make-hasheq (cons (cons '->pict ->pict) rest))))
(define/contract (index type op)
  (->i ((type (and/c tag? has-key?)) (op tag?)) #:pre (type op) (hash-has-key? (hash-ref table type) op) any)
  (hash-ref (hash-ref table type) op))

(struct tagged-object (tag content))

(define (apply-generic op obj . args)
  (apply (index (tagged-object-tag obj) op)
         (tagged-object-content obj)
         args))

(define (->pict obj)
  (apply-generic '->pict obj))
;;--------------------------

(module+ test
  (require sugar/debug rackunit slideshow/base)

  (test-case
      "data"
    (install 'title string? titlet)

    (check-equal? titlet (index 'title '->pict))

    (define (process s) (->pict (tagged-object 'title s)))
    (define (process1 s) (titlet s))
    
    (compare (time-repeat 100 (process "Hello, World!"))
             process
             process1)))
