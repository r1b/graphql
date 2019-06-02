(module lex (lex lex-name)
  (import (chicken base) (chicken format) (clojurian syntax) matchable scheme utf8)

  ; TODO:
  ; * String escapes
  ; * Block strings
  ; * Improve lex-error
  ;     * Indicate the irritant and surrounding context

  ; --------------------------------------------------------------------------

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

  (define (lex-name chars line position #!optional (value '()))
    (match chars
      (((and char (or (? char-alphabetic?) (? char-numeric?) #\_)) tail ...)
       (lex-name tail line (add1 position) (append value `(,char))))
      (_ (cons `(NAME ,(list->string value)) (lex chars line position)))))

  ; FIXME: DRY
  (define (lex-digits chars line position #!optional (value '()))
    (letrec ((lex-digits
               (lambda (chars line position value)
                 (match chars
                   (((and digit (? char-numeric?)) tail ...)
                    (lex-digits tail line (add1 position) (append value `(,digit))))
                   (_ (values chars line position value))))))

      (->* (values chars line position value)

           ((lambda (chars line position value)
              (match chars
                (((and digit (? char-numeric?)) tail ...)
                 (values tail line (add1 position) (append value `(,digit))))
                (_ (lex-error line position)))))

           (lex-digits))))

  (define (lex-number chars line position #!optional (value '()))
    (let ((floatp #f))
      (->* (values chars line position value)

           ((lambda (chars line position value)
              (match chars
                ((#\- tail ...) (values tail
                                        line
                                        (add1 position)
                                        (append value `(,#\-))))
                (_ (values chars line position value)))))

           ((lambda (chars line position value)
              (match chars
                (((and digit (? char-numeric?)) tail ...)
                 (if (char=? digit #\0)
                     (if (char-numeric? (car tail))
                         (lex-error line position)
                         (values tail
                                 line
                                 (add1 position)
                                 (append value `(,digit))))
                     (lex-digits chars
                                 line
                                 position
                                 value)))
                (_ (values chars line position value)))))

           ((lambda (chars line position value)
              (match chars
                ((#\. tail ...)
                 (begin
                   (set! floatp #t)
                   (lex-digits tail
                               line
                               (add1 position)
                               (append value `(,#\.)))))
                (_ (values chars line position value)))))

           ((lambda (chars line position value)
              (match chars
                (((and char (or #\E #\e)) tail ...)
                 (if (or (char=? (car tail) #\+)
                         (char=? (car tail) #\-))
                     (lex-digits (cdr tail)
                                 line
                                 (+ position 2)
                                 (append value `(,char ,(car tail))))
                     (lex-digits tail line (add1 position) (append value `(,char)))))
                (_ (values chars line position value)))))

           ((lambda (chars line position value)
              (cons `(,(if floatp 'FLOAT 'INTEGER) ,(list->string value))
                    (lex chars line position)))))))

  (define (lex-string chars line position #!optional (value '()))
    (match chars
      (((? (compose not source-character?)) _ ...)
       (lex-error line position "Invalid character in string"))
      ((or ((or #\u000A #\u000D) _ ...) ())
       (lex-error line position "Unterminated string"))
      ((#\" tail ...) (cons `(STRING ,(list->string value)) (lex tail line (add1 position))))
      ((char tail ...)
       (lex-string tail line (add1 position) (append value `(,char))))))

  (define (lex-spread chars line position)
    (match chars
      ((#\. #\. tail ...) (cons 'SPREAD (lex tail line (+ position 2))))
      (_ (lex-error line position))))

  ; See https://github.com/graphql/graphql-js/blob/master/src/language/lexer.js
  (define (lex chars #!optional (line 1) (position 0))
    (match chars
      ((#\uFEFF tail ...) (lex tail line (add1 position)))
      (((or #\u0009 #\u0020 #\u002C) tail ...) (lex tail line (add1 position)))
      ((or (#\u000D #\u000A tail ...) ((or #\u000A #\u000D) tail ...))
       (lex tail (add1 line) 0))
      ((#\! tail ...) (cons 'BANG (lex tail line (add1 position))))
      ((#\# comment-tail ...) (lex-comment comment-tail line (add1 position)))
      ((#\$ tail ...) (cons 'DOLLAR (lex tail line (add1 position))))
      ((#\& tail ...) (cons 'AMP (lex tail line (add1 position))))
      ((#\( tail ...) (cons 'PAREN-L (lex tail line (add1 position))))
      ((#\) tail ...) (cons 'PAREN-R (lex tail line (add1 position))))
      ((#\. tail ...) (lex-spread tail line (add1 position)))
      ((#\: tail ...) (cons 'COLON (lex tail line (add1 position))))
      ((#\= tail ...) (cons 'EQUALS (lex tail line (add1 position))))
      ((#\@ tail ...) (cons 'AT (lex tail line (add1 position))))
      ((#\[ tail ...) (cons 'BRACKET-L (lex tail line (add1 position))))
      ((#\] tail ...) (cons 'BRACKET-R (lex tail line (add1 position))))
      ((#\{ tail ...) (cons 'BRACE-L (lex tail line (add1 position))))
      ((#\| tail ...) (cons 'PIPE (lex tail line (add1 position))))
      ((#\} tail ...) (cons 'BRACE-R (lex tail line (add1 position))))
      (((? char-alphabetic?) _ ...) (lex-name chars line position))
      (((or #\- (? char-numeric?)) _ ...) (lex-number chars line position))
      ((#\" tail ...) (lex-string tail line (add1 position)))
      ('() '())
      (_ (lex-error line position)))))
