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
        (lex names-query)))
(test-end)
