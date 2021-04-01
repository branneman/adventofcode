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

    (test-case "pair-exists?"
      (check-equal?
       (pair-exists? '() '(#\a #\b))
       #f)
      (check-equal?
       (pair-exists? '(#\a) '(#\a #\b))
       #f)
      (check-equal?
       (pair-exists? '(#\x #\z #\y) '(#\x #\y))
       #f)
      (check-equal?
       (pair-exists? '(#\x #\y #\z) '(#\x #\y))
       #t)
      (check-equal?
       (pair-exists? '(#\x #\y #\z #\z) '(#\z #\z))
       #t)
      (check-equal?
       (pair-exists? '(#\a #\b #\c #\x #\y #\z) '(#\x #\y))
       #t))

    (test-case "pair-appears-at-least-twice?"
      (check-equal?
       (pair-appears-at-least-twice? "")
       #f)
      (check-equal?
       (pair-appears-at-least-twice? "a")
       #f)
      (check-equal?
       (pair-appears-at-least-twice? "bb")
       #f)
      (check-equal?
       (pair-appears-at-least-twice? "aaa")
       #f)
      (check-equal?
       (pair-appears-at-least-twice? "abcxyz")
       #f)
      (check-equal?
       (pair-appears-at-least-twice? "abcxxyz")
       #f)
      (check-equal?
       (pair-appears-at-least-twice? "xyxy")
       #t)
      (check-equal?
       (pair-appears-at-least-twice? "aaxaa")
       #t)
      (check-equal?
       (pair-appears-at-least-twice? "axybxyc")
       #t)
      (check-equal?
       (pair-appears-at-least-twice? "aershierz")
       #t)
      (check-equal?
       (pair-appears-at-least-twice? "axybxycxyd")
       #t)
      (check-equal?
       (pair-appears-at-least-twice? "aabcdefgaa")
       #t))

    (test-case "at-least-one-repeating-letter-with-letter-in-between?"
      (check-equal?
       (at-least-one-repeating-letter-with-letter-in-between? "")
       #f)
      (check-equal?
       (at-least-one-repeating-letter-with-letter-in-between? "ab")
       #f)
      (check-equal?
       (at-least-one-repeating-letter-with-letter-in-between? "abc")
       #f)
      (check-equal?
       (at-least-one-repeating-letter-with-letter-in-between? "aba")
       #t)
      (check-equal?
       (at-least-one-repeating-letter-with-letter-in-between? "azz")
       #f)
      (check-equal?
       (at-least-one-repeating-letter-with-letter-in-between? "xyx")
       #t)
      (check-equal?
       (at-least-one-repeating-letter-with-letter-in-between? "abcdefeghi")
       #t)
      (check-equal?
       (at-least-one-repeating-letter-with-letter-in-between? "aaa")
       #t))
    
    (test-case "nice-string-part1?"
      (check-equal?
       (nice-string-part1? "aaa")
       #t)
      (check-equal?
       (nice-string-part1? "ugknbfddgicrmopn")
       #t)
      (check-equal?
       (nice-string-part1? "jchzalrnumimnmhp")
       #f)
      (check-equal?
       (nice-string-part1? "haegwjzuvuyypxyu")
       #f)
      (check-equal?
       (nice-string-part1? "dvszwmarrgswjxmb")
       #f))

    (test-case "app part1"
      (check-equal?
       (app nice-string-part1? '("aaa" "abc" "ugknbfddgicrmopn"))
       2))

    (test-case "app part2"
      (check-equal?
       (app nice-string-part2? '("qjhvhtzxzqqjkmpb"
                                 "xxyxx"
                                 "uurcxstgmygtbstg"
                                 "ieodomkazucvgmuy"))
       2))))
