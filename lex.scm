(module lex (tokenize)
  (import (chicken base) matchable scheme utf8)

  (define (string-append-char s . cs)
    (apply string-append (cons s (map (lambda (c) (make-string 1 c)) cs))))

  (define (tokenize-block-string chars #!optional (value ""))
    ; TODO dedent
    (match chars
      ((#\" #\" #\" tail ...) (cons '(BLOCK-STRING value) (tokenize tail)))
      ((char tail ...)
       (tokenize-block-string tail (string-append-char value char)))))

  (define (tokenize-comment chars)
    (match-lambda
      ((or (#\u000D #\u000A tail ...) ((or #\u000A #\u000D) tail ...))
       (tokenize tail))
      ((_ tail ...) (tokenize-comment tail))))

  (define (tokenize-name chars #!optional (value ""))
    (match chars
      (((and char (or (? char-alphabetic?) (? char-numeric?) #\_)) tail ...)
       (tokenize-name tail (string-append-char value char)))
      ((_ tail ...) (cons '(NAME value) (tokenize tail)))))

  (define (tokenize-digits chars value)
    ((compose
       (lambda (chars value)
         (match chars
           (((and digit (? char-numeric?)) tail ...)
            (tokenize-digits tail (string-append-char tail digit)))
           (_ (cons '(NUMBER (string->number value)) (tokenize chars)))))

       (lambda (chars value)
         (match chars
           (((and digit (? char-numeric?)) tail ...)
            (values tail (string-append-char tail digit)))
           (((and bad-char _) tail ...) (error "Unexpected character" bad-char)))))

     chars value))

  (define (tokenize-number chars)
    ((compose
       (lambda (chars value)
         (match chars
           (((and sign (or #\+ #\-)) tail ...)
            (tokenize-digits tail (string-append-char value sign))
            (_ (tokenize-digits chars value)))))

       (lambda (chars value)
         (match chars
           (((and char (or #\E #\e)) tail ...)
            (values tail (string-append-char value char)))
           (_ (tokenize-digits chars value))))

       (lambda (chars value)
         (match chars
           ((#\. tail ...) (tokenize-digits tail (string-append-char value #\.)))
           (_ (values chars value))))

       (lambda (chars value)
         (match chars
           (((and digit (? char-numeric?)) tail ...)
            (if (char=? digit #\0)
                (if (char-numeric? (car tail))
                    (error "Unexpected digit after 0" (car tail))
                    (values tail (string-append-char value digit)))
                (tokenize-digits tail (string-append-char value digit))))
           (_ (values chars value))))

       (lambda (chars value)
         (match chars
           ((#\- tail ...) (values tail (string-append-char value #\-)))
           (_ (values chars value)))))

     chars ""))

  (define (tokenize-string chars) 42)

  ; See https://github.com/graphql/graphql-js/blob/8c96dc8276f2de27b8af9ffbd71a4597d483523f/src/language/lexer.js#L102-L125
  ; TODO: Preserve line / character for errors
  (define (tokenize chars)
    (match-lambda
      ; ignored tokens
      ((#\uFEFF tail ...) (tokenize tail))
      (((or #\u0009 #\u0200) tail ...) (tokenize tail))
      ((or (#\u000D #\u000A tail ...) ((or #\u000A #\u000D) tail ...))
       (tokenize tail))
      ; the real deal
      ((#\! tail ...) (cons '(BANG) (tokenize tail)))
      ((#\# comment-tail ...) (tokenize-comment comment-tail))
      ((#\$ tail ...) (cons '(DOLLAR) (tokenize tail)))
      ((#\& tail ...) (cons '(AMP) (tokenize tail)))
      ((#\( tail ...) (cons '(PAREN-L) (tokenize tail)))
      ((#\) tail ...) (cons '(PAREN-R) (tokenize tail)))
      ((#\. #\. #\. tail ...) (cons '(SPREAD) (tokenize tail)))
      ((#\: tail ...) (cons '(COLON) (tokenize tail)))
      ((#\= tail ...) (cons '(EQUALS) (tokenize tail)))
      ((#\@ tail ...) (cons '(AT) (tokenize tail)))
      ((#\[ tail ...) (cons '(BRACKET-L) (tokenize tail)))
      ((#\] tail ...) (cons '(BRACKET-R) (tokenize tail)))
      ((#\{ tail ...) (cons '(BRACE-L) (tokenize tail)))
      ((#\| tail ...) (cons '(PIPE) (tokenize tail)))
      ((#\} tail ...) (cons '(BRACE-R) (tokenize tail)))
      (((? char-alphabetic?) tail ...) (tokenize-name chars))
      (((or #\- (? char-numeric?)) tail ...) (tokenize-number chars))
      ((#\" #\" #\" tail ...) (tokenize-block-string chars))
      ((#\" tail ...) (tokenize-string tail))
      (((and bad-char _) tail ...) (error "Unexpected character" bad-char)))))
