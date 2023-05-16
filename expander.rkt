#lang racket/base
(require slide-easy/config racket/contract racket/vector slideshow/base (for-syntax racket/base))
(provide program newline statement pos
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
(define (reset s) (make-status #() (hasheq) (hasheq)))
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
(define-syntax-rule (program f ...)
  (foldl (lambda (o i) (collect-garbage 'incremental) (o i)) (make-status #() (hasheq) (hasheq)) (list f ...)))

(define-syntax-rule (newline _ ...) values)

(define-syntax (statement stx)
  (define (read-all p)
    (let loop ((r null))
      (define v (read p))
      (cond ((eof-object? v) (datum->syntax stx (list 'syntax (cons 'begin (reverse r)))))
            (else (loop (cons v r))))))
  (syntax-case stx ()
    ((_ (operator _ operand ...))
     (with-syntax (((operand ...)
                    (map (lambda (o) (let ((c (syntax-e o)))
                                       (cond ((list? c) o)
                                             ((string? c) (read-all (open-input-string c)))
                                             (else (datum->syntax stx (list 'quote o))))))
                         (syntax->list #'(operand ...)))))
       #'(lambda (s) (operator s operand ...))))))

(define-syntax-rule (pos d) 'd)
;;------------------------------------------------------

(module+ test
  (test-case
      "status"
    (define init (make-status #() (hasheq) (hasheq)))
    
    (define result0 ((statement (set "set" test "(text \"hello world\")")) init))
    (define result1 ((statement (send "send" (pos 0) (pos 0) test)) result0))
    (check-eq? (hash-ref (status-table result1) 'test)
               (vector-ref (status-seq result1) 0))
    
    (check-eq? ((statement (exec "exec" "(displayln \"exec : succeed\")")) init)
               init)
    
    (define result2 ((statement (mark "mark" first 0)) init))
    (check-eq? (hash-ref (status-marks result2) 'first) 0)))
