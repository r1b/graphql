(module lexer (lex)
  (import (chicken base) (chicken format) (clojurian syntax) matchable scheme utf8)

  ; TODO:
  ; * String escapes
  ; * Block strings
  ; * Test error cases

  (define-type token-kind (or 'AMP
                              'AT
                              'BANG
                              'BLOCK-STRING
                              'BRACE-L
                              'BRACE-R
                              'BRACKET-L
                              'BRACKET-R
                              'COLON
                              'DOLLAR
                              'EQUALS
                              'FLOAT
                              'INTEGER
                              'PAREN-L
                              'PAREN-R
                              'PIPE
                              'SPREAD
                              'STRING))

  (define-record lex-state (source : string)
                           (line : integer)
                           (line-start : integer)
                           (position : integer))

  (define-record token (kind : token-kind)
                       (start : integer)
                       (end : integer)
                       (line : integer)
                       (column : integer)
                       (value : (or string void)))

  ; --------------------------------------------------------------------------

  ; lex-state

  (define (advance lex-state #!optional (num-chars 1))
    (make-lex-state (lex-state-source lex-state)
                    (lex-state-line lex-state)
                    (lex-state-line-start lex-state)
                    (+ (lex-state-position lex-state) num-chars)))

  (define (advance-line lex-state #!optional (num-chars 1))
    (let ((next-position (+ (lex-state-position lex-state) num-chars)))
      (make-lex-state (lex-state-source lex-state)
                      (add1 (lex-state-line lex-state))
                      next-position
                      next-position)))

  (define (lex-state-column lex-state)
    (- (lex-state-position lex-state)
       (lex-state-line-start lex-state)))

  ; --------------------------------------------------------------------------

  ; tokens

  (define (make-punctuator kind lex-state #!optional (num-chars 1))
    (make-token kind
                (lex-state-position lex-state)
                (+ (lex-state-position lex-state) num-chars)
                (lex-state-line lex-state)
                (lex-state-column lex-state)
                (void)))

  ; FIXME: Stupid name
  (define (make-tok kind lex-state start #!optional (value (void)))
    (make-token kind
                start
                (lex-state-position lex-state)
                (lex-state-line lex-state)
                (lex-state-column lex-state)
                (if (list? value) (list->string value) value)))

  ; --------------------------------------------------------------------------

  ; utils

  (define (source-character? char)
    (or (char>=? char #\u0020)
        (char=? char #\u0009)))

  ; TODO: Indicate the character & surrounding context by looking at the source
  ; in lex-state.
  (define (lex-error lex-state #!optional (message "Unexpected character"))
    (error (sprintf "lex error: [~A,~A] ~A"
                    (lex-state-line lex-state)
                    (lex-state-column lex-state)
                    message)))

  ; --------------------------------------------------------------------------

  (define (lex-comment chars lex-state)
    (match chars
      ((#\u000D #\u000A tail ...) (lex tail (advance-line lex-state 2)))
      (((or #\u000A #\u000D) tail ...) (lex tail (advance-line lex-state)))
      ((_ tail ...) (lex-comment tail (advance lex-state)))))

  (define (lex-name chars
                    lex-state
                    #!optional
                    (start (lex-state-position lex-state))
                    (value '()))
    (match chars
      (((and char (or (? char-alphabetic?) (? char-numeric?) #\_)) tail ...)
       (lex-name tail (advance lex-state) start (append value `(,char))))
      (_ (cons (make-tok 'NAME lex-state start value)
               (lex chars lex-state)))))

  ; FIXME: DRY
  ; FIXME: Just take `start` from top-level context
  (define (lex-digits chars
                      lex-state
                      #!optional
                      (start (lex-state-position lex-state))
                      (value '()))
    (letrec ((lex-digits
               (lambda (chars lex-state start value)
                 (match chars
                   (((and digit (? char-numeric?)) tail ...)
                    (lex-digits tail (advance lex-state) start (append value `(,digit))))
                   (_ (values chars lex-state start value))))))

      (->* (values chars lex-state start value)

           ((lambda (chars lex-state start value)
              (match chars
                (((and digit (? char-numeric?)) tail ...)
                 (values tail (advance lex-state) start (append value `(,digit))))
                (_ (lex-error lex-state)))))

           (lex-digits))))

  ; FIXME: Just take `start` from top-level context
  (define (lex-number chars
                      lex-state
                      #!optional
                      (start (lex-state-position lex-state))
                      (value '()))
    (let ((floatp #f))
      (->* (values chars lex-state start value)

           ((lambda (chars lex-state start value)
              (match chars
                ((#\- tail ...) (values tail
                                        (advance lex-state)
                                        start
                                        (append value `(,#\-))))
                (_ (values chars lex-state start value)))))

           ((lambda (chars lex-state start value)
              (match chars
                (((and digit (? char-numeric?)) tail ...)
                 (if (char=? digit #\0)
                     (if (char-numeric? (car tail))
                         (lex-error lex-state)
                         (values tail
                                 (advance lex-state)
                                 start
                                 (append value `(,digit))))
                     (lex-digits chars
                                 lex-state
                                 start
                                 value)))
                (_ (values chars lex-state start value)))))

           ((lambda (chars lex-state start value)
              (match chars
                ((#\. tail ...)
                 (begin
                   (set! floatp #t)
                   (lex-digits tail
                               (advance lex-state)
                               start
                               (append value `(,#\.)))))
                (_ (values chars lex-state start value)))))

           ((lambda (chars lex-state start value)
              (match chars
                (((and char (or #\E #\e)) tail ...)
                 (if (or (char=? (car tail) #\+)
                         (char=? (car tail) #\-))
                     (lex-digits (cdr tail)
                                 (advance lex-state 2)
                                 start
                                 (append value `(,char ,(car tail))))
                     (lex-digits tail
                                 (advance lex-state)
                                 start
                                 (append value `(,char)))))
                (_ (values chars lex-state start value)))))

           ((lambda (chars lex-state start value)
              (cons (make-tok (if floatp 'FLOAT 'INTEGER)
                              lex-state
                              start
                              value)
                    (lex chars lex-state)))))))

  (define (lex-string chars
                      lex-state
                      #!optional
                      start
                      (value '()))
    (match chars
      (((? (compose not source-character?)) _ ...)
       (lex-error line position "Invalid character in string"))
      ((or ((or #\u000A #\u000D) _ ...) ())
       (lex-error line position "Unterminated string"))
      ((#\" tail ...)
       (cons (make-tok 'STRING (advance lex-state) start value) (lex tail (advance lex-state)))
      ((char tail ...)
       (lex-string tail (advance lex-state) start (append value `(,char))))))

  (define (lex-spread chars lex-state start)
    (match chars
      ((#\. #\. tail ...) (cons (make-tok 'SPREAD (advance lex-state 2) start)
                                (lex tail (advance lex-state 2))))
      (_ (lex-error line position))))

  ; See https://github.com/graphql/graphql-js/blob/master/src/language/lexer.js

  (define (lex chars lex-state)
    (match chars
      ((#\uFEFF tail ...) (lex tail (advance lex-state)))
      (((or #\u0009 #\u0020 #\u002C) tail ...) (lex tail (advance lex-state)))
      ((#\u000D #\u000A tail ...) (lex tail (advance-line lex-state 2)))
      (((or #\u000A #\u000D) tail ...) (lex tail (advance-line lex-state)))
      ((#\! tail ...) (cons (make-punctuator 'BANG lex-state)
                            (lex tail (advance lex-state))))
      ((#\# tail ...) (lex-comment tail (advance lex-state)))
      ((#\$ tail ...) (cons (make-punctuator 'DOLLAR lex-state)
                            (lex tail (advance lex-state))))
      ((#\& tail ...) (cons (make-punctuator 'AMP lex-state)
                            (lex tail (advance lex-state))))
      ((#\( tail ...) (cons (make-punctuator 'PAREN-L lex-state)
                            (lex tail (advance lex-state))))
      ((#\) tail ...) (cons (make-punctuator 'PAREN-R lex-state)
                            (lex tail (advance lex-state))))
      ((#\. tail ...) (lex-spread tail
                                  (advance lex-state)
                                  (lex-state-position lex-state)))
      ((#\: tail ...) (cons (make-punctuator 'COLON lex-state)
                            (lex tail (advance lex-state))))
      ((#\= tail ...) (cons (make-punctuator 'EQUALS lex-state)
                            (lex tail (advance lex-state))))
      ((#\@ tail ...) (cons (make-punctuator 'AT lex-state)
                            (lex tail (advance lex-state))))
      ((#\[ tail ...) (cons (make-punctuator 'BRACKET-L lex-state)
                            (lex tail (advance lex-state))))
      ((#\] tail ...) (cons (make-punctuator 'BRACKET-R lex-state)
                            (lex tail (advance lex-state))))
      ((#\{ tail ...) (cons (make-punctuator 'BRACE-L lex-state)
                            (lex tail (advance lex-state))))
      ((#\| tail ...) (cons (make-punctuator 'PIPE lex-state)
                            (lex tail (advance lex-state))))
      ((#\} tail ...) (cons (make-punctuator 'BRACE-R lex-state)
                            (lex tail (advance lex-state))))
      (((? char-alphabetic?) _ ...) (lex-name chars lex-state)
      (((or #\- (? char-numeric?)) _ ...) (lex-number chars lex-state))
      ((#\" tail ...) (lex-string tail
                                  (advance lex-state)
                                  (lex-state-position lex-state)))
      ('() '())
      (_ (lex-error lex-state))))

  (define (tokenize source)
    ; TODO: It sucks to store another copy of the source in memory. I wonder if
    ; there is a way to use a stream / generator / lazy sequence here. People on
    ; IRC have recommended using `comparse` instead of matchable.
    (lex (string->list source) (make-lex-state source 1 1))))
