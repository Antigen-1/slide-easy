#lang racket/base
(require racket/contract pict)
(provide install ->pict (rename-out (tagged-object tag)))

;;--------------------------
;;the vertical barrier and generic interfaces
(define table (make-hasheq))

(define (tag? o) (and (symbol? o) (symbol-interned? o)))

(define/contract (install type pred ->pict) ;;install a new datatype, which requires a type tag, a predicate, and a `->pict` function
  (->i ((type tag?) (pred (-> any/c any/c)) (->pict (pred) (-> pred pict?))) (result any/c))
  (hash-set! table type (vector pred ->pict)))
(define/contract (get type) ;;reference the `->pict` function specified by the type tag
  (-> tag? any/c)
  (vector-ref (hash-ref table type) 1))
(define/contract (search obj) ;;obtain type tags through predicates
  (-> any/c tag?)
  (let/cc ret
    (hash-for-each
     table
     (lambda (key val)
       (cond (((vector-ref val 0) obj)
              (ret key)))))
    (raise (make-exn:fail (format "Cannot resolve this object: ~s" obj) (current-continuation-marks)))))

(struct tagged-object (tag content))
(struct/c tagged-object tag? any/c)

(define (->pict obj)
  (define (split obj)
    (cond ((tagged-object? obj) (values (tagged-object-tag obj) (tagged-object-content obj)))
          (else (values (search obj) obj))))

  (define-values (tag content) (split obj))
  ((get tag) content))
;;--------------------------

(module+ test
  (require sugar/debug rackunit slideshow/base)

  (test-case
      "data"
    (install 'title string? titlet)

    (define (process s) (->pict (tagged-object 'title s)))
    (define (process1 s) (->pict s))
    
    (compare (time-repeat 100 (process "Hello, World!"))
             process
             process1)
    
    (check-eq? 'title (search "Hello, World!"))))
