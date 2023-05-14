#lang racket/base
(define-syntax-rule (program form ...)
  (begin form ...))

(define-syntax-rule (newline str ...)
  (void))

(define-syntax-rule (statement s) s)

(require slide-easy/config)

(define-namespace-anchor anchor)
(define internal-namespace (module->namespace 'slide-easy/config (namespace-anchor->empty-namespace anchor)))

(define/contract internal-table (hash/c (or/c symbol? exact-nonnegative-integer?) (or/c pict? comment?)) (make-hasheq))
(define/contract internal-sequence (box/c (listof pict?)) (box null))
(define/contract internal-bookmark-table (hash/c symbol? exact-nonnegative-integer?) (make-hasheq))

(define (not-overflow? n)
  (>= (unbox internal-max-size) n))

(define/contract internal-counter (box/c not-overflow?) (box 0))
(define/contract internal-max-size (box/c exact-nonnegative-integer?) (box 0))

(module+ test
  (require rackunit)
  (test-case
      "namespace"
    (check-true (pict? (eval #'(titlet "Hello, World!") internal-namespace)))))

(define-syntax (racket stx)
  (syntax-case stx ()
    ((_ str)
     (with-syntax ((sexp (datum->syntax stx (list 'syntax (let ((p (open-input-string (syntax->datum #'str))))
                                                            (let loop ((r null))
                                                              (define v (read p))
                                                              (cond ((eof-object? v) (cons 'begin (reverse r)))
                                                                    (else (loop (cons v r))))))))))
       #'(eval sexp internal-namespace)))))

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

;;the functions used to handling statements
(define (init) (set-box! internal-sequence null) (set-box! internal-max-size 0) (set-box! internal-counter 0))
(define/contract (jump location) (-> (or/c symbol? exact-nonnegative-integer?) any/c)
  (set-box! internal-counter (let/cc ret (hash-ref internal-bookmark-table (if (symbol? location) location (ret location))))))
(define (send . refs)
  (define seq (unbox internal-sequence))
  
  (define (reference ref) (hash-ref internal-table ref))
  
  (define-values (num subseq) (for/fold ((n 0) (s null))
                                        ((ref (in-list refs)))
                                (values (add1 n) (cons (reference ref) s))))
  (define rsubseq (reverse subseq))
  (define pos (unbox internal-counter))
  (define-values (former latter)
    (split-at seq pos))
  (cond ((>= num (- (unbox internal-max-size) pos))
         (set-box! internal-sequence (append former rsubseq))
         (set-box! internal-max-size (+ pos num)))
        (else (set-box! internal-sequence (append former rsubseq (drop latter num)))))
  (set-box! internal-counter (+ pos num)))
(define (mark sym)
  (hash-set! internal-bookmark-table sym (unbox internal-counter)))
(define (yield)
  ((current-slide-constructor) ((current-pict-combinator) (take (unbox internal-sequence) (unbox internal-counter)))))

(module+ test
  (test-case
      "stack"
    (hash-set! internal-table 'a (t "a"))
    (hash-set! internal-table 'b (t "b"))
    (hash-set! internal-table 12 (t "12"))
    (define test-lst1 (append (make-list 10000 'a) (make-list 10000 'b)))
    (define test-lst2 (make-list 20000 12))

    (displayln "`send` the testing lists")
    (time (apply send test-lst1))
    (jump 1000)
    (mark 'section1)
    (check-eq? (unbox internal-counter) 1000)
    (check-eq? (hash-ref internal-bookmark-table 'section1) 1000)

    (time (apply send test-lst2))
    (check-eq? (unbox internal-counter) 21000)
    
    (jump 'section1)
    (check-eq? (unbox internal-counter) 1000)

    (check-exn exn:fail:contract? (lambda () (jump 30000)))
    
    (init)
    (check-eq? (unbox internal-sequence) null)
    (check-eq? (unbox internal-counter) 0)))

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

(provide program statement newline operand operation definition racket (all-from-out racket/base))
