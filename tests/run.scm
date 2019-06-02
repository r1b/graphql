(include "../lex")
(import lex test utf8)

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

(test-begin)
(test-group "lex"
  (test "lex-name"
        `((NAME ,"query")
          (NAME ,"Foo")
          (BRACE-L)
          (NAME ,"foo")
          (BRACE-L)
          (NAME ,"bar")
          (BRACE-R)
          (BRACE-R))
        (lex names-query))

  (test "lex-comment"
        `((NAME ,"query")
          (NAME ,"Foo")
          (BRACE-L)
          (NAME ,"foo")
          (BRACE-L)
          (NAME ,"bar")
          (BRACE-R)
          (BRACE-R))
        (lex comment-query))

  (test "lex-string"
        `((NAME ,"query")
          (NAME ,"Foo")
          (PAREN-L)
          (NAME ,"baz")
          (COLON)
          (STRING ,"buzz")
          (PAREN-R)
          (BRACE-L)
          (NAME ,"foo")
          (BRACE-L)
          (NAME ,"bar")
          (BRACE-R)
          (BRACE-R))
        (lex string-query))

  (test "lex-spread"
        `((NAME ,"query")
          (NAME ,"Foo")
          (BRACE-L)
          (NAME ,"foo")
          (BRACE-L)
          (SPREAD)
          (NAME ,"Bar_baz")
          (BRACE-R)
          (BRACE-R))
        (lex spread-query))

  (test "lex-number"
        `((NAME ,"query")
          (NAME ,"Foo")
          (PAREN-L)
          (NAME ,"a")
          (COLON)
          (NUMBER ,0)
          (NAME ,"b")
          (COLON)
          (NUMBER ,-2)
          (NAME ,"c")
          (COLON)
          (NUMBER ,3.4)
          (NAME ,"d")
          (COLON)
          (NUMBER ,35000000000.0)
          (NAME ,"e")
          (COLON)
          (NUMBER ,35000000000.0)
          (NAME ,"f")
          (COLON)
          (NUMBER ,0.00035)
          (PAREN-R)
          (BRACE-L)
          (NAME ,"foo")
          (BRACE-L)
          (NAME ,"bar")
          (BRACE-R)
          (BRACE-R))
        (lex numbers-query)))
(test-end)
