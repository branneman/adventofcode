(module no-math racket
  (require rackunit
           "no-math.rkt")

  (module+ test
    (test-case "present->box"
      (check-equal?
       (present->box "2x3x4")
       '(2 3 4))
      (check-equal?
       (present->box "25x14x24")
       '(25 14 24))
      (check-equal?
       (present->box "27x27x22")
       '(27 27 22)))

    (test-case "surface-box+slack"
      (check-equal?
       (surface-box+slack '(2 3 4))
       58)
      (check-equal?
       (surface-box+slack '(3 2 4))
       58)
      (check-equal?
       (surface-box+slack '(1 1 10))
       43)
      (check-equal?
       (surface-box+slack '(10 7 5))
       345)
      (check-equal?
       (surface-box+slack '(26 27 22))
       4308))

    (test-case "take-2-smallest"
      (check-equal?
       (take-2-smallest '(61 37 52 13 52 48))
       '(13 37)))

    (test-case "ribbon-length"
      (check-equal?
       (ribbon-length '(2 3 4))
       34)
      (check-equal?
       (ribbon-length '(1 1 10))
       14))

    (test-case "total-surface"
      (check-equal?
       (total-surface '("2x3x4" "1x1x10"))
       101)
      (check-equal?
       (total-surface '("10x7x5" "26x27x22"))
       4653))))
