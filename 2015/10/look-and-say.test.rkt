(module look-and-say.rkt racket
  (require rackunit
           "look-and-say.rkt")

  (module+ test
    (test-case "number->char"
      (check-equal?
       (number->char 0)
       #\0)
      (check-equal?
       (number->char 1)
       #\1)
      (check-equal?
       (number->char 2)
       #\2))

    (test-case "look-and-say"
      (check-equal?
       (look-and-say "1")
       "11")

      (check-equal?
       (look-and-say "11")
       "21")

      (check-equal?
       (look-and-say "21")
       "1211")

      (check-equal?
       (look-and-say "1211")
       "111221")

      (check-equal?
       (look-and-say "111221")
       "312211"))

    (test-case "call-n"
      (check-equal?
       (call-n 10 0 add1)
       10)

      (check-equal?
       (call-n 15 "" (λ (x) (string-append x "a")))
       "aaaaaaaaaaaaaaa"))))
