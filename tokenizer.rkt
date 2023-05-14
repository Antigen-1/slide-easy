#lang racket/base
(require brag/support)

(define lex
  (lexer-srcloc
   ((:or "init" "jump" "send" "mark" "yield")
    (token lexeme lexeme))
   (blank (token lexeme #:skip? #t))
   ((:+ (:- any-char whitespace (char-set "(){}[]#;,\"'`")))
    (token 'ID (cond ((string->number lexeme))
                     (else (string->symbol lexeme)))))
   ((from/to "@{" "}@")
    (token 'SEXP (trim-ends "@{" lexeme "}@")))
   ((:or "\n" "\r" "\r\n") (token 'NL))))

(module* test #f
  (require rackunit)
  (define (apply-lex str)
    (apply-lexer lex str))

  (test-case
      "lexer"
    (check-equal? (apply-lex "12")
                  (list (srcloc-token (token 'ID 12)
                                      (srcloc 'string 1 0 1 2))))

    (check-equal? (apply-lex "12:14")
                  (list (srcloc-token (token 'ID '12:14)
                                      (srcloc 'string 1 0 1 5))))

    (check-equal? (apply-lex "init")
                  (list (srcloc-token (token "init" "init")
                                      (srcloc 'string 1 0 1 4))))

    (check-equal? (apply-lex " ")
                  (list (srcloc-token (token " " #:skip? #t)
                                      (srcloc 'string 1 0 1 1))))))

(define (make-tokenizer port (path #f))
  (port-count-lines! port)
  (lexer-file-path path)
  (lambda () (lex port)))

(provide make-tokenizer)
