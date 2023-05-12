#lang slideshow
(require racket/contract racket/list)

(define-namespace-anchor anchor)

(define-syntax-rule (program form ...)
  (begin form ...))

(define-syntax-rule (newline str ...)
  (void))

(define-syntax-rule (statement s) s)

(define internal-namespace (module->namespace 'slideshow (namespace-anchor->empty-namespace anchor)))
(define/contract internal-table (hash/c (or/c symbol? exact-nonnegative-integer?) pict?) (make-hasheq))
(define/contract internal-sequence (box/c (listof pict?)) (box null))
(define/contract internal-bookmark-table (hash/c symbol? pict?) (make-hasheq))
(define/contract internal-counter (box/c exact-nonnegative-integer?) (box 0))

(module* test #f
  (require rackunit)
  (test-case
      "namespace"
    (check-true (pict? (eval #'(titlet "Hello, World!") internal-namespace)))))

(define-syntax (definition stx)
  (syntax-case stx ()
    ((_ id sexp)
     (with-syntax ((sexp (datum->syntax stx (list 'syntax (let ((p (open-input-string (syntax->datum #'sexp))))
                                                            (let loop ((r null))
                                                              (define v (read p))
                                                              (cond ((eof-object? v) (cons 'begin (reverse r)))
                                                                    (else (loop (cons v r)))))))))
                   (id (datum->syntax stx (list 'quote #'id))))
       #'(hash-set! internal-table id (eval sexp internal-namespace))))))

(define (init) (set-box! internal-sequence null) (set-box! internal-counter 0))
(define/contract (jump location) (-> (or/c symbol? exact-nonnegative-integer?) any/c)
  (set-box! internal-counter (let/cc ret (hash-ref internal-bookmark-table (if (symbol? location) location (ret location))))))
(define (send . refs)
  (define num (length refs))
  (define-values (former latter)
    (split-at (unbox internal-sequence) (unbox internal-counter)))
  (define-values (_ latter2)
    (split-at latter num))
  (set-box! internal-sequence (append former (map (lambda (ref) (hash-ref internal-table ref)) refs) latter2))
  (set-box! internal-counter (+ (unbox internal-counter) num)))
(define (mark sym)
  (hash-set! internal-bookmark-table sym (unbox internal-counter)))
(define/contract (yield) (-> slide?)
  (apply slide (unbox internal-sequence)))

(define-syntax (operation stx)
  (syntax-case stx (operator)
    ((_ (operator "init"))
     #'(init))
    ((_ (operator "jump") location)
     #'(jump location))
    ((_ (operator "send") ref ...)
     #'(send ref ...))
    ((_ (operator "mark") sym)
     #'(mark sym))
    ((_ (operator "yield"))
     #'(yield))))
(define-syntax-rule (operand id) 'id)

(provide program statement newline operand operation definition (all-from-out slideshow))
