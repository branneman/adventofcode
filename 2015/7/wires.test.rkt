(module wires racket
  (require rackunit
           "wires.rkt")

  (module+ test
    (test-case "tokenise"
      (check-equal?
       (tokenise "lx -> a")
       (list "lx" "->" "a"))

      (check-equal?
       (tokenise "123 -> x")
       (list 123 "->" "x"))

      (check-equal?
       (tokenise "NOT dq -> dr")
       (list 'not "dq" "->" "dr"))

      (check-equal?
       (tokenise "ep OR eo -> eq")
       (list "ep" 'or "eo" "->" "eq"))

      (check-equal?
       (tokenise "hz RSHIFT 5 -> ic")
       (list "hz" 'rshift 5 "->" "ic"))

      (check-equal?
       (tokenise "hz RSHIFT 15 -> ic")
       (list "hz" 'rshift 15 "->" "ic")))

    (test-case "parse"
      (check-equal?
       (parse (list 123 "->" "oh"))
       (operation 'nop '(123) "oh"))

      (check-equal?
       (parse (list "hf" 'or "hl" "->" "hm"))
       (operation 'or '("hf" "hl") "hm"))

      (check-equal?
       (parse (list "gg" 'rshift 2 "->" "hm"))
       (operation 'rshift '("gg" 2) "hm"))

      (check-equal?
       (parse (list 'not 3 "->" "ih"))
       (operation 'not '(3) "ih")))

    (test-case "operator->fn"
      (check-equal?
       ((operator->fn 'nop) 123)
       123)

      (check-equal?
       ((operator->fn 'not) 123)
       65412)

      (check-equal?
       ((operator->fn 'or) 123 456)
       507)

      (check-equal?
       ((operator->fn 'and) 123 456)
       72)

      (check-equal?
       ((operator->fn 'lshift) 123 2)
       492)

      (check-equal?
       ((operator->fn 'rshift) 456 2)
       114))

    (test-case "circuit"
      (check-equal?
       (circuit '("hf OR hl -> hm"))
       (hash "hm" (operation 'or '("hf" "hl") "hm")))

      (check-equal?
       (circuit '("x -> y"
                  "123 -> x"))
       (hash "x" (operation 'nop '(123) "x")
             "y" (operation 'nop '("x") "y"))))

    (test-case "solve"
      (check-equal?
       (solve (hash "x" (operation 'nop '(123) "x")
                    "y" (operation 'nop '("x") "y")))
       (hash "x" 123
             "y" 123))

      (check-equal?
       (solve (hash "y" (operation 'nop '("x") "y")
                    "x" (operation 'nop '(123) "x")))
       (hash "x" 123
             "y" 123))

      (check-equal?
       (solve (hash "a" (operation 'nop '("b") "a")
                    "b" (operation 'nop '("c") "b")
                    "c" (operation 'nop '("d") "c")
                    "d" (operation 'nop '("e") "d")
                    "e" (operation 'nop '("f") "e")
                    "f" (operation 'nop '(123) "f")))
       (hash "a" 123
             "b" 123
             "c" 123
             "d" 123
             "e" 123
             "f" 123)))

    (test-case "app-part1"
      (check-equal?
       (app-part1 '("x -> y"
              "123 -> x"))
       (hash "x" 123
             "y" 123))
      
      (check-equal?
       (app-part1 '("123 -> x"
              "456 -> y"
              "x AND y -> d"
              "x OR y -> e"
              "x LSHIFT 2 -> f"
              "y RSHIFT 2 -> g"
              "NOT x -> h"
              "NOT y -> i"))
       (hash "d" 72
             "e" 507
             "f" 492
             "g" 114
             "h" 65412
             "i" 65079
             "x" 123
             "y" 456))
      
      (check-equal?
       (app-part1 '("x OR y -> e"
              "NOT x -> h"
              "x LSHIFT 2 -> f"
              "y RSHIFT 2 -> g"
              "x AND y -> d"
              "456 -> y"
              "NOT y -> i"
              "123 -> x"))
       (hash "d" 72
             "e" 507
             "f" 492
             "g" 114
             "h" 65412
             "i" 65079
             "x" 123
             "y" 456)))))
