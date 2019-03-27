(module graphql ()
  (import (chicken base) matchable scheme utf8)

  (define (tokenize-block-string chars #!optional (value ""))
    (match chars
      ; FIXME dedent
      ((#\" #\" #\" tail ...) (cons '(BLOCK-STRING value) (tokenize tail)))
      ((char tail ...)
       (tokenize-block-string tail (string-append value (make-string 1 char))))))

  (define (tokenize-comment chars)
    (match-lambda
      ((or (#\u000D #\u000A tail ...) ((or #\u000A #\u000D) tail ...))
       (tokenize tail))
      ((_ tail ...) (tokenize-comment tail))))

  (define (tokenize-name chars) 42)
  (define (tokenize-number chars) 42)
  (define (tokenize-string chars) 42)

  ; See https://github.com/graphql/graphql-js/blob/8c96dc8276f2de27b8af9ffbd71a4597d483523f/src/language/lexer.js#L102-L125
  (define (tokenize chars)
    (match-lambda
      ; ignored tokens
      ((#\uFEFF tail ...) (tokenize tail))
      (((or #\u0009 #\u0200) tail ...) (tokenize tail))
      ((or (#\u000D #\u000A tail ...) ((or #\u000A #\u000D) tail ...))
       (tokenize tail))
      ; the real deal
      ((#\! tail ...) (cons 'BANG (tokenize tail)))
      ((#\# comment-tail ...) (tokenize-comment comment-tail))
      ((#\$ tail ...) (cons 'DOLLAR (tokenize tail)))
      ((#\& tail ...) (cons 'AMP (tokenize tail)))
      ((#\( tail ...) (cons 'PAREN-L (tokenize tail)))
      ((#\) tail ...) (cons 'PAREN-R (tokenize tail)))
      ((#\. #\. #\. tail ...) (cons 'SPREAD (tokenize tail)))
      ((#\: tail ...) (cons 'COLON (tokenize tail)))
      ((#\= tail ...) (cons 'EQUALS (tokenize tail)))
      ((#\@ tail ...) (cons 'AT (tokenize tail)))
      ((#\[ tail ...) (cons 'BRACKET-L (tokenize tail)))
      ((#\] tail ...) (cons 'BRACKET-R (tokenize tail)))
      ((#\{ tail ...) (cons 'BRACE-L (tokenize tail)))
      ((#\| tail ...) (cons 'PIPE (tokenize tail)))
      ((#\} tail ...) (cons 'BRACE-R (tokenize tail)))
      (((? char-alphabetic?) tail ...) (tokenize-name chars))
      (((? char-numeric?) tail ...) (tokenize-number chars))
      ((#\" #\" #\" tail ...) (tokenize-block-string chars))
      ((#\" tail ...) (tokenize-string tail))
      (((and bad-char _) tail ...) (error "tokenize: unexpected character" bad-char)))))
