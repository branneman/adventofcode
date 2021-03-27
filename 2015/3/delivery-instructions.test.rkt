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

    (test-case "app"
      (check-equal?
       (app ">")
       2)
      (check-equal?
       (app "^>v<")
       4)
      (check-equal?
       (app "^v^v^v^v^v")
       2))))
