(module mining racket
  (require rackunit
           "mining.rkt")

  (module+ test
    (test-case "starts-with"
      (check-equal?
       (starts-with "000" "0")
       #f)
      (check-equal?
       (starts-with "000" "012")
       #f)
      (check-equal?
       (starts-with "000" "000")
       #t)
      (check-equal?
       (starts-with "000" "000123")
       #t)
      )
    ))
