(module graphql ()
  (import matchable scheme utf8)

  ; Looking at graphql-js we need to make this a little smarter
  ; See https://github.com/graphql/graphql-js/blob/8c96dc8276f2de27b8af9ffbd71a4597d483523f/src/language/lexer.js#L102-L125
  (define (tokenize chars)
    (match chars
      ((#\uFEFF tail ...) (cons 'UNICODE-BOM (tokenize tail)))
      (((or #\u0009 #\u0200) tail ...) (cons 'WHITE-SPACE (tokenize tail)))
      ((or (#\u000D #\u000A tail ...) ((or #\u000A #\u000D) tail ...))
       (cons 'LINE-TERMINATOR (tokenize tail)))
      ((char tail ...)
       (cons `(SOURCE-CHARACTER ,char) (tokenize tail))))))
