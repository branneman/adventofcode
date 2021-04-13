(module matchsticks racket
  (require rackunit
           "matchsticks.rkt")

  (module+ test
    (test-case "str-length"
      (check-equal?
       (str-length "\"\"")
       2)
      (check-equal?
       (str-length "\"abc\"")
       5)
      (check-equal?
       (str-length "\"aaa\\\"aaa\"")
       10)
      (check-equal?
       (str-length "\"\\x27\"")
       6))

    (test-case "str-length-in-memory"
      (check-equal?
       (str-length-in-memory "\"\"")
       0)
      (check-equal?
       (str-length-in-memory "\"abc\"")
       3)
      (check-equal?
       (str-length-in-memory "\"aaa\\\"aaa\"")
       7)
      (check-equal?
       (str-length-in-memory "\"\\x27\"")
       1))

    (test-case "str-length-code-representation"
      (check-equal?
       (str-length-code-representation "\"\"")
       6)
      (check-equal?
       (str-length-code-representation "\"abc\"")
       9)
      (check-equal?
       (str-length-code-representation "\"aaa\\\"aaa\"")
       16)
      (check-equal?
       (str-length-code-representation "\"\\x27\"")
       11))

    (test-case "app-part1"
      (check-equal?
       (app-part1 (list "\"\""
                        "\"abc\""
                        "\"aaa\\\"aaa\""
                        "\"\\x27\""))
       12))
    
    (test-case "app-part2"
      (check-equal?
       (app-part2 (list "\"\""
                        "\"abc\""
                        "\"aaa\\\"aaa\""
                        "\"\\x27\""))
       19))))
