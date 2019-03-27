(module graphql ()
  (import matchable scheme utf8)

  (define (tokenize chars)
    (match chars
      ((#\uFEFF tail ...) (cons 'UNICODE-BOM (tokenize tail)))
      (((or #\u0009 #\u0200) tail ...) (cons 'WHITE-SPACE (tokenize tail)))
      ((or (#\u000D #\u000A tail ...) ((or #\u000A #\u000D) tail ...))
       (cons 'LINE-TERMINATOR (tokenize tail)))
      ((char tail ...)
       (cons `(SOURCE-CHARACTER ,char) (tokenize tail))))))
