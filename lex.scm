(module lex (lex)
  (import (chicken base) matchable scheme utf8)

  (define (string-append-char s . cs)
    (apply string-append (cons s (map (cut make-string 1 <>) cs))))

  (define (lex-block-string chars #!optional (value ""))
    ; TODO dedent
    ; FIXME this is like wrong
    (match chars
      ((#\" #\" #\" tail ...) (cons '(BLOCK-STRING value) (lex tail)))
      ((char tail ...)
       (lex-block-string tail (string-append-char value char)))))

  (define (lex-comment chars)
    (match-lambda
      ((or (#\u000D #\u000A tail ...) ((or #\u000A #\u000D) tail ...))
       (lex tail))
      ((_ tail ...) (lex-comment tail))))

  (define (lex-name chars #!optional (value ""))
    (match chars
      (((and char (or (? char-alphabetic?) (? char-numeric?) #\_)) tail ...)
       (lex-name tail (string-append-char value char)))
      ((_ tail ...) (cons '(NAME value) (lex tail)))))

  (define (lex-digits chars value)
    ((compose
       (lambda (chars value)
         (match chars
           (((and digit (? char-numeric?)) tail ...)
            (lex-digits tail (string-append-char tail digit)))
           (_ (cons '(NUMBER (string->number value)) (lex chars)))))

       (lambda (chars value)
         (match chars
           (((and digit (? char-numeric?)) tail ...)
            (values tail (string-append-char tail digit)))
           (((and bad-char _) tail ...) (error "Unexpected character" bad-char)))))

     chars value))

  (define (lex-number chars)
    ((compose
       (lambda (chars value)
         (match chars
           (((and sign (or #\+ #\-)) tail ...)
            (lex-digits tail (string-append-char value sign))
            (_ (lex-digits chars value)))))

       (lambda (chars value)
         (match chars
           (((and char (or #\E #\e)) tail ...)
            (values tail (string-append-char value char)))
           (_ (lex-digits chars value))))

       (lambda (chars value)
         (match chars
           ((#\. tail ...) (lex-digits tail (string-append-char value #\.)))
           (_ (values chars value))))

       (lambda (chars value)
         (match chars
           (((and digit (? char-numeric?)) tail ...)
            (if (char=? digit #\0)
                (if (char-numeric? (car tail))
                    (error "Unexpected digit after 0" (car tail))
                    (values tail (string-append-char value digit)))
                (lex-digits tail (string-append-char value digit))))
           (_ (values chars value))))

       (lambda (chars value)
         (match chars
           ((#\- tail ...) (values tail (string-append-char value #\-)))
           (_ (values chars value)))))

     chars ""))

  (define (lex-string chars)
    )

  ; See https://github.com/graphql/graphql-js/blob/8c96dc8276f2de27b8af9ffbd71a4597d483523f/src/language/lexer.js#L102-L125
  ; TODO: Preserve line / character for errors
  (define (lex chars)
    (match-lambda
      ; ignored tokens
      ((#\uFEFF tail ...) (lex tail))
      (((or #\u0009 #\u0200) tail ...) (lex tail))
      ((or (#\u000D #\u000A tail ...) ((or #\u000A #\u000D) tail ...))
       (lex tail))
      ; the real deal
      ((#\! tail ...) (cons '(BANG) (lex tail)))
      ((#\# comment-tail ...) (lex-comment comment-tail))
      ((#\$ tail ...) (cons '(DOLLAR) (lex tail)))
      ((#\& tail ...) (cons '(AMP) (lex tail)))
      ((#\( tail ...) (cons '(PAREN-L) (lex tail)))
      ((#\) tail ...) (cons '(PAREN-R) (lex tail)))
      ; FIXME handle dot followed by invalid char
      ((#\. #\. #\. tail ...) (cons '(SPREAD) (lex tail)))
      ((#\: tail ...) (cons '(COLON) (lex tail)))
      ((#\= tail ...) (cons '(EQUALS) (lex tail)))
      ((#\@ tail ...) (cons '(AT) (lex tail)))
      ((#\[ tail ...) (cons '(BRACKET-L) (lex tail)))
      ((#\] tail ...) (cons '(BRACKET-R) (lex tail)))
      ((#\{ tail ...) (cons '(BRACE-L) (lex tail)))
      ((#\| tail ...) (cons '(PIPE) (lex tail)))
      ((#\} tail ...) (cons '(BRACE-R) (lex tail)))
      (((? char-alphabetic?) tail ...) (lex-name chars))
      (((or #\- (? char-numeric?)) tail ...) (lex-number chars))
      ((#\" #\" #\" tail ...) (lex-block-string chars))
      ((#\" tail ...) (lex-string tail))
      (((and bad-char _) tail ...) (error "Unexpected character" bad-char)))))
