(module delivery-instructions racket
  (require rackunit
           "delivery-instructions.rkt")

  (module+ test
    (test-case "unique"
      (check-equal?
       (unique '((1 1) (1 0) (0 0)))
       '((1 1) (1 0) (0 0)))
      (check-equal?
       (unique '((0 0) (1 0) (0 0)))
       '((1 0) (0 0)))
      (check-equal?
       (unique '((0 0) (1 0) (1 -1) (0 -1) (0 0)))
       '((1 0) (1 -1) (0 -1) (0 0)))
      (check-equal?
       (unique '((0 0) (0 -1) (0 0) (0 -1) (0 0) (0 -1) (0 0) (0 -1) (0 0) (0 -1) (0 0)))
       '((0 -1) (0 0))))

    (test-case "move"
      (check-equal?
       (move '(0 0) #\^)
       '(0 -1))
      (check-equal?
       (move '(0 0) #\>)
       '(1 0))
      (check-equal?
       (move '(0 0) #\v)
       '(0 1))
      (check-equal?
       (move '(0 0) #\<)
       '(-1 0)))

    (test-case "delivery-run"
      (check-equal?
       (delivery-run '(#\>))
       '((1 0) (0 0)))
      (check-equal?
       (delivery-run '(#\^ #\> #\v #\<))
       '((0 0) (1 0) (1 -1) (0 -1) (0 0)))
      (check-equal?
       (delivery-run '(#\^ #\v #\^ #\v #\^ #\v #\^ #\v #\^ #\v))
       '((0 0) (0 -1) (0 0) (0 -1) (0 0) (0 -1) (0 0) (0 -1) (0 0) (0 -1) (0 0))))

    (test-case "unzip"
      (check-equal?
       (unzip '(1 a 2 b 3 c))
       '((1 2 3) (a b c)))
      (check-equal?
       (unzip '(1 a 2 b 3 c 4))
       '((1 2 3 4) (a b c)))
      (check-equal?
       (unzip '((1 1) (a a) (2 2) (b b) (3 3) (c c)))
       '(((1 1) (2 2) (3 3)) ((a a) (b b) (c c))))
      )

    (test-case "count-deliveries"
      (check-equal?
       (count-deliveries '((0 0) (1 0) (1 -1) (0 -1) (0 0))
                         '((1 1) (1 0) (2 0) (1 0) (0 0)))
       6))

    (test-case "app"
      (check-equal?
       (app "^v")
       3)
      (check-equal?
       (app "^>v<")
       3)
      (check-equal?
       (app "^v^v^v^v^v")
       11))))
