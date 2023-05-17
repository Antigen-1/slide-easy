#lang racket/base
(require slide-easy/config racket/contract racket/vector slideshow/base (for-syntax racket/base racket/syntax))
(provide program line-separator statement pos mod
         reset set mark exec send yield
         (all-from-out racket/base))

(define-namespace-anchor anchor)
(define namespace (module->namespace 'slide-easy/config (namespace-anchor->empty-namespace anchor)))

(module+ test
  (require rackunit)
  (test-case
      "namespace"
    (check-true (pict? (eval #'(text "Hello, World") namespace)))))

;;------------------------------------------------------
;;the core datatype and its contract
(struct status (seq marks table) #:extra-constructor-name make-status)
(struct/c status
          (vectorof (-> pict? pict?))
          (hash/c symbol? exact-nonnegative-integer?)
          (hash/c symbol? (-> pict? pict?)))
;;------------------------------------------------------

;;------------------------------------------------------
;;functions
(define (reset s) (make-status (vector) (hasheq) (hasheq)))
(define (set s sym form) (struct-copy status s (table (hash-set (status-table s) sym (eval form namespace)))))
(define (mark s sym pos) (struct-copy status s (marks (hash-set (status-marks s) sym pos))))
(define (exec s form) (eval form namespace) s)

(define (get-position s p) (if (exact-nonnegative-integer? p) p (hash-ref (status-marks s) p)))

(define (send s start end . refs)
  (define st (get-position s start))
  (define ed (get-position s end))

  (define (reference ref) (hash-ref (status-table s) ref))
  
  (define new (list->vector (map reference refs)))
  (define nlen (vector-length new))
  (define seq (status-seq s))
  (define len (vector-length seq))
  (struct-copy status s (seq (let ((vec (make-vector (- (+ nlen len) (- ed st)))))
                               (vector-copy! vec 0 seq 0 st)
                               (vector-copy! vec st new)
                               (vector-copy! vec (+ st nlen) seq ed)
                               vec))))
(define (yield s start end)
  (define st (get-position s start))
  (define ed (get-position s end))
  (define lst ((if (left-to-right?) reverse values) (vector->list (vector-copy (status-seq s) st ed))))
  (define pic ((apply compose values lst) (current-init-pict)))
  (slide pic)
  s)
;;------------------------------------------------------

;;------------------------------------------------------
;;macros
(begin-for-syntax
  (define (read-all p)
    (let loop ((r null))
      (define v (read p))
      (cond ((eof-object? v) (reverse r))
            (else (loop (cons v r)))))))

(define-syntax (mod stx)
  (syntax-case stx ()
    ((_ _ program)
     #'program)
    ((_ _ form _ program)
     (let ((libs (read-all (open-input-string (syntax->datum #'form)))))
       (with-syntax (((id ...) (datum->syntax stx (map (lambda (_) (generate-temporary 'lib)) libs)))
                     ((lib ...) (datum->syntax stx libs)))
         #'(begin (require racket/runtime-path (for-syntax racket/base))
                  (define-runtime-module-path-index id 'lib) ...
                  (parameterize ((current-namespace namespace)) (namespace-require (module-path-index-resolve id))) ...
                  program))))))

(define-syntax-rule (program f ...)
  (foldl (lambda (o i) (collect-garbage 'incremental) (o i)) (make-status (vector) (hasheq) (hasheq)) (list f ...)))

(define-syntax-rule (line-separator _ ...) values)

(define-syntax (statement stx)
  (syntax-case stx ()
    ((_ (operator _ operand ...))
     (with-syntax (((operand ...)
                    (map (lambda (o) (let ((c (syntax-e o)))
                                       (cond ((list? c) o)
                                             ((string? c) (datum->syntax stx (list 'syntax (cons 'begin (read-all (open-input-string c))))))
                                             (else (datum->syntax stx (list 'quote o))))))
                         (syntax->list #'(operand ...)))))
       #'(lambda (s) (operator s operand ...))))))

(define-syntax-rule (pos d) 'd)
;;------------------------------------------------------

(module+ test
  (test-case
      "status"
    (define init (make-status (vector) (hasheq) (hasheq)))
    (check-eq? init ((line-separator) init))
    
    (define result0 ((statement (set "set" test "(text \"hello world\")")) init))
    (define result1 ((statement (send "send" (pos 0) (pos 0) test)) result0))
    (check-eq? (hash-ref (status-table result1) 'test)
               (vector-ref (status-seq result1) 0))
    (let ((result3 ((statement (reset "reset")) result1)))
      (check-equal? (status-seq result3) (vector))
      (check-equal? (status-marks result3) (hasheq))
      (check-equal? (status-table result3) (hasheq)))
    
    (check-eq? ((statement (exec "exec" "(displayln \"exec : succeed\")")) init)
               init)
    
    (define result2 ((statement (mark "mark" first 0)) init))
    (check-eq? (hash-ref (status-marks result2) 'first) 0)))
