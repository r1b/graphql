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
        (lex spread-query)))
(test-end)
