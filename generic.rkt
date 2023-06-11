#lang racket/base
(require racket/contract pict racket/vector)
;;there are no contracts outside this `provide` form, while contracts for representations are still placed into the table
(provide (contract-out #:unprotected-submodule unsafe
                       #:exists (hierarchies tagged)
                       (rename has-key? installed?
                               (-> hierarchies boolean?))
                       
                       (install
                        (opt/c (->i ((type (and/c hierarchies
                                                  (not/c has-key?)
                                                  (or/c root?
                                                        (and/c
                                                         (lambda (l) (> (depth l) 1))
                                                         (lambda (l) (tag? (current l)))
                                                         (lambda (l) (has-key? (super l)))))))
                                     (contract contract?)
                                     (coerce (type) (-> any/c (if (root? type) pict? (get-contract (super type))))))
                                    #:rest (rest (listof (cons/c tag? any/c)))
                                    any)))
                       #; (assign (-> has-key? (cons/c tag? any/c) ... any)) ;;suppress users from mutating methods after they are installed due to security question marks
                       (rename assign attach
                               (opt/c (->i ((type (and/c hierarchies has-key?)))
                                           #:rest (rest (type) (listof (cons/c (and/c tag? (not/c (lambda (op) (index type op #f)))) any/c)))
                                           any)))
                       (apply-generic
                        (opt/c (->i ((op tag?)
                                     (obj tagged))
                                    #:rest (rest list?)
                                    #:pre (obj op rest)
                                    (let ((r (index (type obj) op #f)))
                                      (and r (procedure? r) (procedure-arity-includes? r (add1 (length rest)))))
                                    any)))
                       (coerce (opt/c (->i ((object (dest) (and/c tagged
                                                                  (or/c (not/c dest)
                                                                        (lambda (o) (has-tag? (type o) dest)))))
                                            (dest (or/c #f tag?)))
                                           (result (or/c tagged pict?)))))
                       (->pict (-> tagged any))
                       
                       #; (tagged-object? (-> any/c boolean?)) ;;it always returns false because of the abstract contract
                       (tag? (-> any/c boolean?))
                       (tag (opt/c (->i ((type (and/c hierarchies has-key?)) (content (type) (get-contract type))) (result tagged))))
                       (type (-> tagged hierarchies))

                       ;;primitives for handling types
                       ;;they never check if types are installed
                       (make-type (-> (or/c tag? hierarchies) ... hierarchies))
                       (ref (opt/c (->i ((type (dep) (and/c hierarchies (lambda (l) (> (depth l) dep)))) (dep exact-nonnegative-integer?)) (result tag?))))
                       (sub (opt/c (->i ((type hierarchies)
                                         (start exact-nonnegative-integer?)
                                         (end (type start) (and/c exact-nonnegative-integer?
                                                                  (lambda (e) (and (>= e start) (< e (depth type)))))))
                                        (result hierarchies))))
                       (depth (-> hierarchies exact-nonnegative-integer?))
                       (has-tag? (-> hierarchies tag? boolean?))

                       ;;these operations are not primitives, but they still never check if types are installed 
                       (current (-> (and/c hierarchies (lambda (l) (exact-positive-integer? (depth l)))) tag?))
                       (super (-> (and/c hierarchies (not/c root?)) hierarchies))
                       (root? (-> hierarchies boolean?)))
         )

;;--------------------------
;;the vertical barrier and generic interfaces
(define table (make-hash))

(define (tag? o) (and (symbol? o) (symbol-interned? o)))
(define (has-key? p) (hash-has-key? table p))

(struct tagged-object (type content) #:transparent)

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

;;resolve the hierarchies of types
(define (make-type . types)
  (let loop ((types types) (result '#()))
    (cond ((null? types) result)
          ((vector? (car types)) (loop (cdr types) (vector-append result (car types))))
          (else (loop (cdr types) (vector-append result (vector (car types))))))))
(define (sub type depth1 depth2)
  (vector-copy type depth1 depth2))
(define (ref type depth)
  (vector-ref type depth))
(define (depth type)
  (vector-length type))
(define (has-tag? type tag)
  (let/cc abort
    (for ((e (in-vector type)))
      (cond ((eq? e tag)
             (abort #t))))
    #f))

;;aliases for compatibility
(define content ;;retrieve contents
  tagged-object-content)
(define tag ;;tag objects
  tagged-object)
(define type ;;retrieve type tags
  tagged-object-type)
(define (current type) ;;get the first tag
  (ref type 0))
(define (super type) ;;get the super type
  (sub type 1 (vector-length type)))
(define (root? type) ;;find out whether or not this type is a root type
  (= 1 (depth type)))

(define (apply-generic op obj . rest) ;;call the function with the object's content and other by-position arguments
  (apply (index (type obj) op) (content obj) rest))
(define (coerce obj dest) ;;coerce the content of the object
  (let loop ((types (type obj)) (content (content obj)))
    (cond ((and (root? types) (not dest)) ((get-coerce types) content))
          ((eq? dest (current types)) (tag types content))
          (else (loop (super types) ((get-coerce types) content))))))

(define (->pict obj)
  (coerce obj #f))
;;--------------------------

(module+ test
  (require sugar/debug rackunit slideshow/base)

  (test-case
      "data"
    (install (make-type 'pict) pict? values)
    (install (make-type 'title 'pict) string? titlet (cons 'length string-length))

    (check-equal? string-length (index (make-type 'title 'pict) 'length))
    (check-equal? titlet (get-coerce (make-type 'title 'pict)))
    (check-equal? string? (get-contract (make-type 'title 'pict)))

    (assign (make-type 'title 'pict) (cons 'length (compose add1 string-length)))
    (check-eq? (apply-generic 'length (tag (make-type 'title 'pict) "abc")) 4)

    (define (process s) (titlet s)) 
    (define (process1 s) (coerce (tag (make-type 'title 'pict) s) #f))
    (define (process2 s) (coerce (tag (make-type 'pict) (titlet s)) #f))
    (define (process3 s) (coerce (coerce (tag (make-type 'title 'pict) s) 'pict) #f))
    
    (compare (time-repeat 100000 (process "Hello, World!"))
             process
             process1
             process2
             process3)))
