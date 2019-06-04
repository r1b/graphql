(include "../lexer")
(import lexer test utf8)

(define names-query (string->list #<<END
query Foo {
    foo {
        bar
    }
}
END
  ))

(define comment-query (string->list #<<END
query Foo {
    foo {
        bar  # TODO: baz
    }
}
END
  ))

(define string-query (string->list #<<END
query Foo(baz: "buzz") {
    foo {
        bar
    }
}
END
  ))

(define spread-query (string->list #<<END
query Foo {
    foo {
        ...Bar_baz
    }
}
END
  ))

(define numbers-query (string->list #<<END
query Foo(a: 0, b: -2, c: 3.4, d: 3.5e10, e: 3.5e+10, f: 3.5E-4) {
    foo {
        bar
    }
}
END
  ))

(define punctuators-query (string->list #<<END
union Whatsit = Who | Dat
type Buzz & implements Qux {}

query Foo($n: Int = 1, $things: [String]) {
    foo {
        bar @baz()
    }
}
END
  ))

(test-begin)
(test-group "lex"
  (test "lex-name"
        `((NAME ,"query")
          (NAME ,"Foo")
          BRACE-L
          (NAME ,"foo")
          BRACE-L
          (NAME ,"bar")
          BRACE-R
          BRACE-R)
        (lex names-query))

  (test "lex-comment"
        `((NAME ,"query")
          (NAME ,"Foo")
          BRACE-L
          (NAME ,"foo")
          BRACE-L
          (NAME ,"bar")
          BRACE-R
          BRACE-R)
        (lex comment-query))

  (test "lex-string"
        `((NAME ,"query")
          (NAME ,"Foo")
          PAREN-L
          (NAME ,"baz")
          COLON
          (STRING ,"buzz")
          PAREN-R
          BRACE-L
          (NAME ,"foo")
          BRACE-L
          (NAME ,"bar")
          BRACE-R
          BRACE-R)
        (lex string-query))

  (test "lex-spread"
        `((NAME ,"query")
          (NAME ,"Foo")
          BRACE-L
          (NAME ,"foo")
          BRACE-L
          SPREAD
          (NAME ,"Bar_baz")
          BRACE-R
          BRACE-R)
        (lex spread-query))

  (test "lex-number"
        `((NAME ,"query")
          (NAME ,"Foo")
          PAREN-L
          (NAME ,"a")
          COLON
          (INTEGER ,"0")
          (NAME ,"b")
          COLON
          (INTEGER ,"-2")
          (NAME ,"c")
          COLON
          (FLOAT ,"3.4")
          (NAME ,"d")
          COLON
          (FLOAT ,"3.5e10")
          (NAME ,"e")
          COLON
          (FLOAT ,"3.5e+10")
          (NAME ,"f")
          COLON
          (FLOAT ,"3.5E-4")
          PAREN-R
          BRACE-L
          (NAME ,"foo")
          BRACE-L
          (NAME ,"bar")
          BRACE-R
          BRACE-R)
        (lex numbers-query))
  (test "misc punctuators"
        `((NAME ,"union")
          (NAME ,"Whatsit")
          EQUALS
          (NAME ,"Who")
          PIPE
          (NAME ,"Dat")
          (NAME ,"type")
          (NAME ,"Buzz")
          AMP
          (NAME ,"implements")
          (NAME ,"Qux")
          BRACE-L
          BRACE-R
          (NAME ,"query")
          (NAME ,"Foo")
          PAREN-L
          DOLLAR
          (NAME ,"n")
          COLON
          (NAME ,"Int")
          EQUALS
          (INTEGER "1")
          DOLLAR
          (NAME ,"things")
          COLON
          BRACKET-L
          (NAME ,"String")
          BRACKET-R
          PAREN-R
          BRACE-L
          (NAME "foo")
          BRACE-L
          (NAME "bar")
          AT
          (NAME "baz")
          PAREN-L
          PAREN-R
          BRACE-R
          BRACE-R)
        (lex punctuators-query)))
(test-end)
