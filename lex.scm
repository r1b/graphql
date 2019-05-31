(module lex (lex)
  (import (chicken base) clojurian matchable scheme utf8)

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
    (->* (values chars value)

         (lambda (chars value)
           (match chars
             (((and digit (? char-numeric?)) tail ...)
              (values tail (string-append-char tail digit)))
             ((_ tail ...) (lex-error line position))))

         (lambda (chars value)
           (match chars
             (((and digit (? char-numeric?)) tail ...)
              (lex-digits tail (string-append-char tail digit)))
             (_ (cons '(NUMBER (string->number value)) (lex chars)))))))

  (define (lex-number chars line position)
    (->* (values chars "" line position)

         (lambda (chars value line position)
           (match chars
             ((#\- tail ...) (values tail
                                     (string-append-char value #\-)
                                     line
                                     (add1 position))))
           (_ (values chars value line position))))

         (lambda (chars value line position)
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
             (_ (values chars value line position))))

         (lambda (chars value line position)
           (match chars
             ((#\. tail ...) (lex-digits tail
                                         (string-append-char value #\.)
                                         line
                                         (add1 position)))
             (_ (values chars value line position))))

         (lambda (chars value line position)
           (match chars
             (((and char (or #\E #\e)) tail ...)
              (values tail
                      (string-append-char value char)
                      line
                      (add1 position)))
             (_ (lex-digits chars value line position))))

         (lambda (chars value line position)
           (match chars
             (((and sign (or #\+ #\-)) tail ...)
              (lex-digits tail
                          (string-append-char value sign)
                          line
                          (add1 position))
              (_ (lex-digits chars value line position)))))))



  (define (lex-string chars line position #!optional (value ""))
    (match chars
      (((? (-> source-character? not)) _ ...)
       (lex-error line position "Invalid character in string"))
      ((or ((or #\u000A #\u000D) _ ...) ())
       (lex-error line position "Unterminated string"))
      ((#\" tail ...) (cons '(STRING value) (lex tail line (add1 position))))
      ((char tail ...)
       (lex-string tail line (add1 position (string-append-char value char))))))

  (define (lex-spread chars line position)
    (match chars
      ((#\. #\. tail ...) (cons '(SPREAD) (lex tail line (+ position 2))))
      ((_ tail ...) (lex-error line position))))

  ; See https://github.com/graphql/graphql-js/blob/master/src/language/lexer.js
  (define (lex chars #!optional (line 1) (position 0))
    (match-lambda
      ; ignored tokens
      ((#\uFEFF tail ...) (lex tail line (add1 position)))
      (((or #\u0009 #\u0200 #\u002C) tail ...) (lex tail line (add1 position)))
      ((or (#\u000D #\u000A tail ...) ((or #\u000A #\u000D) tail ...))
       (lex tail (add1 line) 0))
      ; the real deal
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
      ((_ ...) (lex-error line position)))))
