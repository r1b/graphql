(module lex (lex lex-name)
  (import (chicken base) (chicken format) (clojurian syntax) matchable scheme utf8)

  ; TODO:
  ; * String escapes
  ; * Block strings

  ; --------------------------------------------------------------------------

  (define (string-append-char s . cs)
    (apply string-append (cons s (map (cut make-string 1 <>) cs))))

  (define (source-character? char)
    (or (char>=? char #\u0020)
        (char=? char #\u0009)))

  (define (lex-error line position #!optional (message "Unexpected character"))
    (error (sprintf "[~A:~A] ~A" line position message)))

  ; --------------------------------------------------------------------------

  (define (lex-comment chars line position)
    (match chars
      ((or (#\u000D #\u000A tail ...) ((or #\u000A #\u000D) tail ...))
       (lex tail))
      ((#\u000D #\u000A tail ...) (lex-comment tail line (+ position 2)))
      (((or #\u000A #\u000D) tail ...) (lex-comment tail line (add1 position)))
      ((_ tail ...) (lex-comment tail line (add1 position)))))

  (define (lex-name chars line position #!optional (value ""))
    (match chars
      (((and char (or (? char-alphabetic?) (? char-numeric?) #\_)) tail ...)
       (lex-name tail line (add1 position) (string-append-char value char)))
      (_ (cons `(NAME ,value) (lex chars line position)))))

  (define (lex-digits chars line position #!optional (value ""))
    (->* (values chars value)

         ((lambda (chars value)
           (match chars
             (((and digit (? char-numeric?)) tail ...)
              (values tail (string-append-char tail digit)))
             ((_ tail ...) (lex-error line position)))))

         ((lambda (chars value)
           (match chars
             (((and digit (? char-numeric?)) tail ...)
              (lex-digits tail line (add1 position) (string-append-char tail digit)))
             (_ (cons '(NUMBER (string->number value)) (lex chars))))))))

  (define (lex-number chars line position #!optional (value ""))
    (->* (values chars "" line position)

         ((lambda (chars value line position)
           (match chars
             ((#\- tail ...) (values tail
                                     (string-append-char value #\-)
                                     line
                                     (add1 position)))
             (_ (values chars value line position)))))

         ((lambda (chars value line position)
           (match chars
             (((and digit (? char-numeric?)) tail ...)
              (if (char=? digit #\0)
                  (if (char-numeric? (car tail))
                      (lex-error line position)
                      (values tail
                              (string-append-char value digit)
                              line
                              (add1 position)))
                  (lex-digits tail
                              (string-append-char value digit)
                              line
                              (add1 position))))
             (_ (values chars value line position)))))

         ((lambda (chars value line position)
           (match chars
             ((#\. tail ...) (lex-digits tail
                                         (string-append-char value #\.)
                                         line
                                         (add1 position)))
             (_ (values chars value line position)))))

         ((lambda (chars value line position)
           (match chars
             (((and char (or #\E #\e)) tail ...)
              (values tail
                      (string-append-char value char)
                      line
                      (add1 position)))
             (_ (lex-digits chars value line position)))))

         ((lambda (chars value line position)
           (match chars
             (((and sign (or #\+ #\-)) tail ...)
              (lex-digits tail
                          (string-append-char value sign)
                          line
                          (add1 position)))
              (_ (lex-digits chars value line position)))))))


  (define (lex-string chars line position #!optional (value ""))
    (match chars
      (((? (compose not source-character?)) _ ...)
       (lex-error line position "Invalid character in string"))
      ((or ((or #\u000A #\u000D) _ ...) ())
       (lex-error line position "Unterminated string"))
      ((#\" tail ...) (cons `(STRING ,value) (lex tail line (add1 position))))
      ((char tail ...)
       (lex-string tail line (add1 position) (string-append-char value char)))))

  (define (lex-spread chars line position)
    (match chars
      ((#\. #\. tail ...) (cons '(SPREAD) (lex tail line (+ position 2))))
      ((_ tail ...) (lex-error line position))))

  ; See https://github.com/graphql/graphql-js/blob/master/src/language/lexer.js
  (define (lex chars #!optional (line 1) (position 0))
    (match chars
      ((#\uFEFF tail ...) (lex tail line (add1 position)))
      (((or #\u0009 #\u0020 #\u002C) tail ...) (lex tail line (add1 position)))
      ((or (#\u000D #\u000A tail ...) ((or #\u000A #\u000D) tail ...))
       (lex tail (add1 line) 0))
      ((#\! tail ...) (cons '(BANG) (lex tail line (add1 position))))
      ((#\# comment-tail ...) (lex-comment comment-tail line (add1 position)))
      ((#\$ tail ...) (cons '(DOLLAR) (lex tail line (add1 position))))
      ((#\& tail ...) (cons '(AMP) (lex tail line (add1 position))))
      ((#\( tail ...) (cons '(PAREN-L) (lex tail line (add1 position))))
      ((#\) tail ...) (cons '(PAREN-R) (lex tail line (add1 position))))
      ((#\. tail ...) (lex-spread tail line (add1 position)))
      ((#\: tail ...) (cons '(COLON) (lex tail line (add1 position))))
      ((#\= tail ...) (cons '(EQUALS) (lex tail line (add1 position))))
      ((#\@ tail ...) (cons '(AT) (lex tail line (add1 position))))
      ((#\[ tail ...) (cons '(BRACKET-L) (lex tail line (add1 position))))
      ((#\] tail ...) (cons '(BRACKET-R) (lex tail line (add1 position))))
      ((#\{ tail ...) (cons '(BRACE-L) (lex tail line (add1 position))))
      ((#\| tail ...) (cons '(PIPE) (lex tail line (add1 position))))
      ((#\} tail ...) (cons '(BRACE-R) (lex tail line (add1 position))))
      (((? char-alphabetic?) _ ...) (lex-name chars line position))
      (((or #\- (? char-numeric?)) _ ...) (lex-number chars line position))
      ((#\" tail ...) (lex-string tail line (add1 position)))
      ('() '())
      ((_ ...) (lex-error line position)))))
