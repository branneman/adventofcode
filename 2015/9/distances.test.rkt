(module distances racket
  (require rackunit
           "distances.rkt")

  (module+ test
    (test-case "parse"
      (check-equal?
       (parse "London to Dublin = 464")
       (list "Dublin" "London" 464))

      (check-equal?
       (parse "London to Belfast = 518")
       (list "Belfast" "London" 518))

      (check-equal?
       (parse "Dublin to Belfast = 141")
       (list "Belfast" "Dublin" 141)))

    (test-case "unique-locations"
      (check-equal?
       (unique-locations (list (list "London" "Dublin" 464)
                               (list "Dublin" "Belfast" 141)))
       (list "Belfast" "Dublin" "London"))

      (check-equal?
       (unique-locations (list (list "Faerun" "Straylight" 137)
                               (list "Faerun" "AlphaCentauri" 3)
                               (list "Tristram" "Tambi" 63)
                               (list "Tristram" "Arbre" 14)))
       (list "AlphaCentauri" "Arbre" "Faerun" "Straylight" "Tambi" "Tristram")))

    (test-case "distance-path"
      (check-equal?
       (distance-path "Dublin" "London"
                      (list (list "Dublin" "London" 464)
                            (list "Belfast" "London" 518)))
       464)

      (check-equal?
       (distance-path "London" "Dublin"
                      (list (list "Dublin" "London" 464)
                            (list "Belfast" "London" 518)))
       464))

    (test-case "distance-route"
      (check-equal?
       (distance-route (list "a" "b" "c" "d")
                       (list (list "a" "b" 1)
                             (list "b" "c" 2)
                             (list "c" "d" 3)))
       6)

      (check-equal?
       (distance-route (list "Dublin" "London" "Belfast")
                       (list (list "Dublin" "London" 464)
                             (list "Belfast" "London" 518)))
       982))

    (test-case "distances"
      (check-equal?
       (distances (list (list "Dublin" "London" 464)
                        (list "Belfast" "London" 518)
                        (list "Belfast" "Dublin" 141)))
       (list 605 659 982 659 982 605)))

    (test-case "app"
      (check-equal?
       (app (list "London to Dublin = 464"
                  "London to Belfast = 518"
                  "Dublin to Belfast = 141"))
       605))))
