#lang racket/base
(require racket/contract pict)
(provide install apply-generic ->pict (rename-out (tagged-object tag) (tagged-object-content content)))

;;--------------------------
;;the vertical barrier and generic interfaces
(define table (make-hash))

(define (tag? o) (and (symbol? o) (symbol-interned? o)))
(define (already-has? t) (let/cc ret (hash-for-each table (lambda (key val) (cond ((eq? (car key) t) (ret #t))))) #f))
(define (has-key? p) (hash-has-key? table p))

(define/contract (install type contract ->pict . rest) ;;install a new datatype
  (->i ((type (and/c tag? (not/c already-has?))) (contract contract?) (->pict (contract) (-> contract pict?)))
       #:rest (rest (contract) (listof (cons/c tag? (-> contract any/c ... any))))
       any)
  (hash-set! table (cons type '->pict) ->pict)
  (map (lambda (pair) (hash-set! table (cons type (car pair)) (cdr pair))) rest))
(define/contract (index type op)
  (->i ((type tag?) (op tag?)) #:pre (type op) (has-key? (cons type op)) any)
  (hash-ref table (cons type op)))

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
