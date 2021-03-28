(module intern-elves racket
  (require rackunit
           "intern-elves.rkt")

  (module+ test
    (test-case "vowel?"
      (check-equal? (vowel? #\a) #t)
      (check-equal? (vowel? #\e) #t)
      (check-equal? (vowel? #\i) #t)
      (check-equal? (vowel? #\o) #t)
      (check-equal? (vowel? #\u) #t)
      (check-equal? (vowel? #\t) #f)
      (check-equal? (vowel? #\E) #f))

    (test-case "at-least-three-vowels?"
      (check-equal?
       (at-least-three-vowels? "axi")
       #f)
      (check-equal?
       (at-least-three-vowels? "aei")
       #t)
      (check-equal?
       (at-least-three-vowels? "xazegov")
       #t)
      (check-equal?
       (at-least-three-vowels? "aeiouaeiouaeiou")
       #t)
      (check-equal?
       (at-least-three-vowels? "bcdfghjklmnpqrstvwxyz")
       #f))

    (test-case "at-least-one-letter-twice-in-a-row?"
      (check-equal?
       (at-least-one-letter-twice-in-a-row? "")
       #f)
      (check-equal?
       (at-least-one-letter-twice-in-a-row? "a")
       #f)
      (check-equal?
       (at-least-one-letter-twice-in-a-row? "aa")
       #t)
      (check-equal?
       (at-least-one-letter-twice-in-a-row? "abc")
       #f)
      (check-equal?
       (at-least-one-letter-twice-in-a-row? "abbc")
       #t))

    (test-case "contains-some?"
      (check-equal?
       (contains-some? '("a" "b" "c") "xya")
       #t)
      (check-equal?
       (contains-some? '("aa" "bb" "cc") "xxbbyyaa")
       #t)
      (check-equal?
       (contains-some? '("a" "b" "c") "xyz")
       #f)
      (check-equal?
       (contains-some? '("ab" "cd" "xy") "haegwjzuvuyypxyu")
       #t))
    
    (test-case "nice-string?"
      (check-equal?
       (nice-string? "aaa")
       #t)
      (check-equal?
       (nice-string? "ugknbfddgicrmopn")
       #t)
      (check-equal?
       (nice-string? "jchzalrnumimnmhp")
       #f)
      (check-equal?
       (nice-string? "haegwjzuvuyypxyu")
       #f)
      (check-equal?
       (nice-string? "dvszwmarrgswjxmb")
       #f))

    (test-case "app"
      (check-equal?
       (app '("aaa"
              "abc"
              "ugknbfddgicrmopn"))
       2))))
