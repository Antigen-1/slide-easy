#lang racket/base
(require slide-easy/config racket/contract racket/list slideshow/base sugar/list (for-syntax racket/base))
(provide program newline statement (all-from-out racket/base))

(define-namespace-anchor anchor)
(define namespace (module->namespace 'slide-easy/config (namespace-anchor->empty-namespace anchor)))

(module+ test
  (require rackunit)
  (test-case
      "namespace"
    (check-true (pict? (eval #'(text "Hello, World") namespace)))))

(struct status (seq marks table mode last))
(struct/c status (listof (-> (or/c pict? #f) pict?)) (hash/c symbol? exact-nonnegative-integer?) (hash/c symbol? (-> (or/c pict? #f) pict?)) (or/c 'reslide 'new) (or/c #f pict?))

(define (reset s) (status null (hasheq) (hasheq) 'new (current-init-pict)))
(define (set s sym form) (struct-copy status s (table (hash-set (status-table s) sym (eval form namespace)))))
(define (mark s sym pos) (struct-copy status s (marks (hash-set (status-marks s) sym pos))))
(define (exec s form) (eval form namespace) s)

(define (get-position s p) (if (exact-nonnegative-integer? p) p (hash-ref (status-marks s) p)))

(define (send s start end . refs)
  (let-values (((former latter) (split-at (status-seq s) (get-position s end))))
    (let ((_former (take former (get-position s start)))
          (reference (lambda (ref) (hash-ref (status-table s) ref))))
      (struct-copy status s (seq (append _former (map reference refs) latter))))))
(define (yield s start end)
  (define st (get-position s start))
  (define ed (get-position s end))
  (define lst ((if (left-to-right?) reverse values) (sublist (status-seq s) st ed)))
  (define pic ((apply compose values lst) (status-last s)))
  (case (status-mode s)
    ((reslide) (define last (retract-most-recent-slide))
               (re-slide last pic)
               (struct-copy status s (last pic)))
    ((new) (slide pic)
           (struct-copy status s (mode 'reslide) (last pic)))))

(define-syntax (slide-begin stx)
  (syntax-case stx ()
    ((_ status step0 step ...)
     #'(slide-begin (step0 status) step ...))
    ((_ _)
     #'(void))))

(define-syntax-rule (program f ...)
  (slide-begin (status null (hasheq) (hasheq) 'new (current-init-pict)) f ...))

(define-syntax-rule (newline _ ...) values)

(define-syntax (statement stx)
  (define (read-all p)
    (let loop ((r null))
      (define v (read p))
      (cond ((eof-object? v) (datum->syntax stx (list 'syntax (cons 'begin (reverse r))))) (else (loop (cons v r))))))
  (syntax-case stx (set mark exec send yield reset pos)
    ((_ (set "set" id form))
     (with-syntax ((sexp (read-all (open-input-string (syntax->datum #'form)))))
       #'(lambda (s) (set s 'id sexp))))
    ((_ (mark "mark" id pos))
     #'(lambda (s) (mark s 'id pos)))
    ((_ (exec "exec" form))
     (with-syntax ((sexp (read-all (open-input-string (syntax->datum #'form)))))
       #'(lambda (s) (exec s sexp))))
    ((_ (send "send" (pos start) (pos end) ref ...))
     #'(lambda (s) (send s start end ref ...)))
    ((_ (yield "yield" (pos start) (pos end)))
     #'(lambda (s) (yield s start end)))
    ((_ (reset "reset"))
     #'(lambda (s) (reset s)))))
