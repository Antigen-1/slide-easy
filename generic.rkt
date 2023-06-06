#lang racket/base
(require racket/contract pict)
(provide (contract-out #:unprotected-submodule unsafe
                       (install
                        (opt/c (->i ((type (and/c (not/c has-key?)
                                                  (or/c tag?
                                                        (list/c tag? (and/c tag? has-key?))
                                                        (and/c (cons/c tag? (cons/c tag? (non-empty-listof tag?))) (lambda (k) (has-key? (cdr k)))))))
                                     (contract contract?)
                                     (coerce (type) (-> any/c (if (tag? type) pict? (get-contract (cdr type))))))
                                    #:rest (rest (listof (cons/c tag? any/c)))
                                    any)))
                       (assign (-> has-key? (cons/c tag? any/c) ... any))
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
                       (tag (opt/c (->i ((type has-key?) (content (type) (get-contract type))) (result tagged-object?))))
                       (coerce (opt/c (->i ((object (dest) (struct/dc tagged-object
                                                                      (tag (if dest
                                                                               (lambda (l) (findf (lambda (t) (eq? t dest)) l))
                                                                               any/c))
                                                                      (content any/c))))
                                           ((dest (or/c #f tag?)))
                                           any)))
                       (type (-> tagged-object? any)))
         (contract-out ;;`attach` and `->pict` is not included in the `unsafe` submodule
          (rename coerce ->pict
                  (-> tagged-object? any))
          (rename assign attach
                  (opt/c (->i ((type has-key?))
                              #:rest (rest (type) (listof (cons/c (and/c tag? (not/c (lambda (op) (index type op #f)))) any/c)))
                              any))))
         )

;;--------------------------
;;the vertical barrier and generic interfaces
(define table (make-hash))

(define (tag? o) (and (symbol? o) (symbol-interned? o)))
(define (has-key? p) (hash-has-key? table p))

(struct tagged-object (tag content) #:transparent)

(define (install type contract coerce . rest) ;;install datatypes
  (hash-set! table type (vector contract coerce (make-hasheq rest))))
(define (assign type . rest) ;;modify fuctions
  (map (lambda (pair) (hash-set! (vector-ref (hash-ref table type) 2) (car pair) (cdr pair))) rest))
(define (index type op (fail (lambda () (raise (make-exn:fail:contract "Cannot resolve this operation" (current-continuation-marks)))))) ;;find functions
  (hash-ref (vector-ref (hash-ref table type) 2) op fail))
(define (get-contract type) ;;find contracts
  (vector-ref (hash-ref table type) 0))
(define (get-coerce type) ;;get `coerce` functions
  (vector-ref (hash-ref table type) 1))

;;alias for compatibility
(define content ;;retrieve contents
  tagged-object-content)
(define tag ;;tag objects
  tagged-object)
(define type ;;retrieve type tags
  tagged-object-tag)

(define (apply-generic op obj . rest) ;;call the function with the object's content and other by-position arguments
  (apply (index (type obj) op) (content obj) rest))
(define (coerce obj (dest #f)) ;;coerce the content of the object
  (let loop ((types (type obj)) (content (content obj)))
    (cond ((and (tag? types) (not dest)) ((get-coerce types) content))
          ((and (tag? types) (eq? types dest)) (tag types content))
          ((and (null? (cddr types)) (not dest))
           ((get-coerce (cadr types)) ((get-coerce types) content)))
          ((and (null? (cddr types)) (eq? dest (cadr types)))
           (tag (cadr types) ((get-coerce types) content)))
          ((eq? dest (car types)) (tag types content))
          (else (loop (cdr types) ((get-coerce types) content))))))
;;--------------------------

(module+ test
  (require sugar/debug rackunit slideshow/base)

  (test-case
      "data"
    (install 'pict pict? values)
    (install '(title pict) string? titlet (cons 'length string-length))

    (check-equal? string-length (index '(title pict) 'length))
    (check-equal? titlet (get-coerce '(title pict)))
    (check-equal? string? (get-contract '(title pict)))

    (assign '(title pict) (cons 'length (compose add1 string-length)))
    (check-eq? (apply-generic 'length (tag '(title pict) "abc")) 4)

    (define (process s) (titlet s)) 
    (define (process1 s) (coerce (tag '(title pict) s)))
    (define (process2 s) (coerce (tag 'pict (titlet s))))
    (define (process3 s) (coerce (coerce (tag '(title pict) s) 'pict)))
    
    (compare (time-repeat 10000 (process "Hello, World!"))
             process
             process1
             process2
             process3)))
