#lang racket

(provide app)

(module+ test (require rackunit))

(define (app xs)
  (let* ([happiness (map parse xs)]
         [perms (permutations (unique-names happiness))]
         [xs (map (λ (x) (total-hapiness x happiness)) perms)])
    (car (sort xs >))))
(module+ test
  (check-equal?
   (app (list "Alice would gain 54 happiness units by sitting next to Bob."
              "Alice would lose 79 happiness units by sitting next to Carol."
              "Alice would lose 2 happiness units by sitting next to David."
              "Bob would gain 83 happiness units by sitting next to Alice."
              "Bob would lose 7 happiness units by sitting next to Carol."
              "Bob would lose 63 happiness units by sitting next to David."
              "Carol would lose 62 happiness units by sitting next to Alice."
              "Carol would gain 60 happiness units by sitting next to Bob."
              "Carol would gain 55 happiness units by sitting next to David."
              "David would gain 46 happiness units by sitting next to Alice."
              "David would lose 7 happiness units by sitting next to Bob."
              "David would gain 41 happiness units by sitting next to Carol."))
   330))

(define (total-hapiness xs hapiness)
  (for/sum ([x (get-direct-siblings xs)])
    (caddr (findf (λ (y) (and (equal? (car x) (car y))
                              (equal? (cadr x) (cadr y))))
                  hapiness))))
(module+ test
  (check-equal?
   (total-hapiness (list "Alice" "Bob" "Carol" "David")
                   (list (list "Alice" "David" -2)
                         (list "Alice" "Bob" 54)
                         (list "Bob" "Alice" 83)
                         (list "Bob" "Carol" -7)
                         (list "Carol" "Bob" 60)
                         (list "Carol" "David" 55)
                         (list "David" "Carol" 41)
                         (list "David" "Alice" 46)))
   330))

(define (get-direct-siblings xs)
  (define l (length xs))
  (reverse
   (remove-duplicates
    (let recur ([n 0] [acc empty])
      (cond
        [(or (= n l) (< (length xs) 2))
         acc]
        [else
         (let ([prev (list (list-ref xs n)
                           (cond [(= n 0) (list-ref xs (sub1 l))]
                                 [else (list-ref xs (sub1 n))]))]
               [next (list (list-ref xs n)
                           (cond [(= n (sub1 l)) (car xs)]
                                 [else (list-ref xs (add1 n))]))])
           (recur (add1 n) (cons next (cons prev acc))))])))))
(module+ test
  (check-equal?
   (get-direct-siblings empty)
   empty)
  (check-equal?
   (get-direct-siblings (list "a"))
   empty)
  (check-equal?
   (get-direct-siblings (list "a" "b"))
   (list (list "a" "b")
         (list "b" "a")))
  (check-equal?
   (get-direct-siblings (list "a" "b" "c"))
   (list (list "a" "c")
         (list "a" "b")
         (list "b" "a")
         (list "b" "c")
         (list "c" "b")
         (list "c" "a")))
  (check-equal?
   (get-direct-siblings (list "a" "b" "c" "d"))
   (list (list "a" "d")
         (list "a" "b")
         (list "b" "a")
         (list "b" "c")
         (list "c" "b")
         (list "c" "d")
         (list "d" "c")
         (list "d" "a"))))

(define parse-re
  #px"(\\w+) would (\\w+) (\\d+) happiness units by sitting next to (\\w+).")
(define (parse s)
  (let* ([l (regexp-match parse-re s)]
         [p1 (list-ref l 1)]
         [p2 (list-ref l 4)]
         [n (cond [(equal? "gain" (list-ref l 2))
                   (string->number (list-ref l 3))]
                  [(equal? "lose" (list-ref l 2))
                   (- (string->number (list-ref l 3)))])])
    (list p1 p2 n)))
(module+ test
  (check-equal?
   (parse "Alice would gain 54 happiness units by sitting next to Bob.")
   (list "Alice" "Bob" 54))
  (check-equal?
   (parse "Carol would lose 62 happiness units by sitting next to Alice.")
   (list "Carol" "Alice" -62)))

(define (unique-names xs)
  (let ([flat (foldl (λ (x acc) (cons (car x) (cons (cadr x) acc)))
                     empty xs)])
    (sort (remove-duplicates flat) string<?)))
(module+ test
  (check-equal?
   (unique-names (list (list "a" "b") (list "c" "a")))
   (list "a" "b" "c"))
  (check-equal?
   (unique-names (list (list "foo" "bar" 4) (list "bar" "baz" 2)))
   (list "bar" "baz" "foo")))
