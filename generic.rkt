#lang racket/base
(require racket/contract pict)
(provide (contract-out #:unprotected-submodule unsafe ;;the structure is still protected though
                       (install
                        (opt/c (-> (and/c tag? (not/c has-key?)) contract? (-> any/c pict?) (cons/c (and/c tag? (not/c '->pict)) any/c) ... any)))
                       (rename assign attach
                               (opt/c (->i ((tag (and/c tag? has-key?)))
                                           #:rest (rest (tag) (listof (cons/c (and/c tag? (not/c (lambda (op) (index tag op #f)))) any/c)))
                                           any)))
                       (assign
                        (opt/c (->i ((_ (and/c tag? has-key?)))
                                    #:rest (rest (listof (cons/dc (op tag?) (proc (op) (if (eq? op '->pict) (-> any/c pict?) any/c)))))
                                    any)))
                       (apply-generic
                        (opt/c (->i ((op tag?)
                                     (obj (rest op)
                                          (and/c
                                           tagged-object?
                                           (lambda (o)
                                             (let ((r (index (type o) op #f)))
                                               (and r (procedure? r) (procedure-arity-includes? r (add1 (length rest)))))))))
                                    #:rest (rest list?)
                                    any)))
                       (tagged-object? (-> any/c boolean?))
                       (->pict (-> tagged-object? any))
                       (content (-> tagged-object? any))
                       (tag (opt/c (->i ((type (and/c tag? has-key?)) (content (type) (get-contract type))) (result tagged-object?))))
                       (type (-> tagged-object? any))))

;;--------------------------
;;the vertical barrier and generic interfaces
(define table (make-hasheq))

(define (tag? o) (and (symbol? o) (symbol-interned? o)))
(define (has-key? p) (hash-has-key? table p))

(struct tagged-object (tag content))

(define (install type contract ->pict . rest) ;;install a new datatype
  (hash-set! table type (vector contract (make-hasheq (cons (cons '->pict ->pict) rest)))))
(define (assign type . rest) ;;modify fuctions
  (map (lambda (pair) (hash-set! (vector-ref (hash-ref table type) 1) (car pair) (cdr pair))) rest))
(define (index type op (fail (lambda () (raise (make-exn:fail:contract "Cannot resolve this operation" (current-continuation-marks)))))) ;;find functions
  (hash-ref (vector-ref (hash-ref table type) 1) op fail))
(define (get-contract type) ;;find contracts
  (vector-ref (hash-ref table type) 0))

;;alias for compatibility
(define content ;;retrieve contents
  tagged-object-content)
(define tag ;;tag objects
  tagged-object)
(define type ;;retrieve type tags
  tagged-object-tag)

(define (apply-generic op obj . rest) ;;call the function with the object's content and other by-position arguments
  (apply (index (type obj) op) (content obj) rest))

(define (->pict obj)
  (apply-generic '->pict obj))
;;--------------------------

(module+ test
  (require sugar/debug rackunit slideshow/base)

  (test-case
      "data"
    (install 'title string? titlet)

    (check-equal? titlet (index 'title '->pict))

    (define (process s) (->pict (tag 'title s)))
    (define (process1 s) (titlet s))
    
    (compare (time-repeat 1000 (process "Hello, World!"))
             process
             process1)))
