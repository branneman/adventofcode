(module fire-hazard racket
  (require rackunit
           "fire-hazard.rkt")

  (module+ test
    (test-case "make-grid"
      (check-equal?
       (make-grid 3 0)
       #(#(0 0 0)
         #(0 0 0)
         #(0 0 0)))
      (check-equal?
       (make-grid 5 #f)
       #(#(#f #f #f #f #f)
         #(#f #f #f #f #f)
         #(#f #f #f #f #f)
         #(#f #f #f #f #f)
         #(#f #f #f #f #f))))

    (test-case "grid-cell"
      (check-equal?
       (grid-cell #(#(0 1 2)
                    #(3 4 5)
                    #(6 7 8)) 0 0)
       0)
      (check-equal?
       (grid-cell #(#(0 1 2)
                    #(3 4 5)
                    #(6 7 8)) 1 1)
       4)
      (check-equal?
       (grid-cell #(#(0 1 2)
                    #(3 4 5)
                    #(6 7 8)) 2 2)
       8))

    (test-case "grid-cell!"
      (check-equal?
       (grid-cell! (make-grid 3 #f) 0 1 #t)
       #(#(#f #t #f)
         #(#f #f #f)
         #(#f #f #f)))
      (check-equal?
       (grid-cell! (make-grid 3 #f) 1 1 #t)
       #(#(#f #f #f)
         #(#f #t #f)
         #(#f #f #f)))
      (check-equal?
       (grid-cell! (make-grid 3 #f) 1 2 #t)
       #(#(#f #f #f)
         #(#f #f #t)
         #(#f #f #f))))

    (test-case "flip-switch"
      (check-equal?
       (flip-switch 0 'turn-on)
       1)
      (check-equal?
       (flip-switch 0 'turn-off)
       0)
      (check-equal?
       (flip-switch 1 'turn-off)
       0)
      (check-equal?
       (flip-switch 1 'turn-on)
       2)
      (check-equal?
       (flip-switch 0 'toggle)
       2)
      (check-equal?
       (flip-switch 1 'toggle)
       3))

    (test-case "execute"
      (check-equal?
       (execute (make-grid 5 0)
                (action 'turn-on 0 0 4 4))
       (make-grid 5 1))

      (check-equal?
       (execute (make-grid 5 1)
                (action 'turn-off 0 0 4 4))
       (make-grid 5 0))

      (check-equal?
       (execute (make-grid 3 0)
                (action 'toggle 1 1 1 1))
       #(#(0 0 0)
         #(0 2 0)
         #(0 0 0)))

      (check-equal?
       (execute (list->vector (list (list->vector '(0 0 0))
                                    (list->vector '(1 1 1))
                                    (list->vector '(0 0 0))))
                (action 'toggle 0 0 2 2))
       #(#(2 2 2)
         #(3 3 3)
         #(2 2 2))))

    (test-case "action->symbol"
      (check-equal?
       (action->symbol "turn on")
       'turn-on)
      (check-equal?
       (action->symbol "turn off")
       'turn-off)
      (check-equal?
       (action->symbol "toggle")
       'toggle))

    (test-case "parse"
      (check-equal?
       (parse "turn on 0,0 through 999,999")
       (action 'turn-on 0 0 999 999))
      (check-equal?
       (parse "toggle 0,0 through 999,0")
       (action 'toggle 0 0 999 0))
      (check-equal?
       (parse "turn off 499,499 through 500,500")
       (action 'turn-off 499 499 500 500)))

    (test-case "sum-2d-vector"
      (check-equal?
       (sum-2d-vector #(#(0 0 0)
                        #(0 0 0)
                        #(0 0 0)))
       0)
      (check-equal?
       (sum-2d-vector #(#(1 2 3)
                        #(4 5 6)
                        #(7 8 9)))
       45)
      (check-equal?
       (sum-2d-vector #(#(0 0 1)
                        #(1 0 1)
                        #(0 0 1)))
       4))

    (test-case "app"
      (check-equal?
       (app '("turn on 0,0 through 999,999"))
       1000000)
      (check-equal?
       (app '("toggle 0,0 through 999,999"))
       2000000)
      (check-equal?
       (app '("turn on 0,0 through 999,0"))
       1000)
      (check-equal?
       (app '("turn on 499,499 through 500,500"))
       4)
      (check-equal?
       (app '("turn on 0,0 through 999,999"
              "toggle 0,0 through 999,0"))
       1002000)
      (check-equal?
       (app '("turn on 500,0 through 500,999"
              "turn on 501,0 through 501,999"
              "turn on 0,500 through 999,500"
              "turn on 0,501 through 999,501"))
       4000))))
