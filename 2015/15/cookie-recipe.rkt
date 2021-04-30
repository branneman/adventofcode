#lang racket

(provide winning-cookie-score)

(module+ test (require rackunit))

(struct ingredient (capacity durability flavor texture calories) #:transparent)

(define (cookie-permutations n)
  (let ([lst empty]
        [0-n (in-range 0 (add1 n))])
    (for ([x 0-n])
      (for ([y 0-n])
        (for ([z 0-n])
          (for ([a 0-n])
            (let ([combination (list x y z a)])
              (when (= n (apply + combination))
                (set! lst (cons combination lst))))))))
    (cookie-sort lst)))
(module+ test
  (test-case "cookie-permutations"
    (check-equal?
     (cookie-permutations 1)
     '((0 0 0 1)
       (0 0 1 0)
       (0 1 0 0)
       (1 0 0 0)))
    (check-equal?
     (cookie-permutations 2)
     '((0 0 0 2)
       (0 0 1 1)
       (0 0 2 0)
       (0 1 0 1)
       (0 1 1 0)
       (0 2 0 0)
       (1 0 0 1)
       (1 0 1 0)
       (1 1 0 0)
       (2 0 0 0)))))

(define (cookie-sort xs)
  (sort xs (λ (x y)
             (cond
               ; first 3 positions are equal: compare 4th
               [(and (= (car x) (car y))
                     (= (cadr x) (cadr y))
                     (= (caddr x) (caddr y)))
                (< (cadddr x) (cadddr y))]

               ; first 2 positions are equal: compare 3rd
               [(and (= (car x) (car y))
                     (= (cadr x) (cadr y)))
                (< (caddr x) (caddr y))]

               ; first positions are equal: compare 2nd
               [(= (car x) (car y))
                (< (cadr x) (cadr y))]

               ; no positions are equal: compare 1st
               [else
                (< (car x) (car y))]))))
(module+ test
  (test-case "cookie-sort"
    (check-equal?
     (cookie-sort '((1 2 3) (2 3 1) (3 1 2)))
     '((1 2 3) (2 3 1) (3 1 2)))

    (check-equal?
     (cookie-sort '((4 3 2) (2 3 10) (3 1 2)))
     '((2 3 10) (3 1 2) (4 3 2)))

    (check-equal?
     (cookie-sort '((4 3 2) (2 3 10) (2 3 2)))
     '((2 3 2) (2 3 10) (4 3 2)))

    (check-equal?
     (cookie-sort '((2 1 1 1) (1 2 1 1) (1 1 2 1) (1 1 1 2)))
     '((1 1 1 2) (1 1 2 1) (1 2 1 1) (2 1 1 1)))))

(define (property->accessor s)
  (hash-ref (hash "capacity" ingredient-capacity
                  "durability" ingredient-durability
                  "flavor" ingredient-flavor
                  "texture" ingredient-texture
                  "calories" ingredient-calories)
            s))

(define (property-score permutation property ingredients)
  (max 0 (apply + (map (λ (x) (* (cadr x)
                                 ((property->accessor property)
                                  (hash-ref ingredients (car x)))))
                       permutation))))
(module+ test
  (test-case "property-score"
    (let ([permutation (list (list "Butterscotch" 44) (list "Cinnamon" 56))])
      (let ([ingredients (hash "Butterscotch" (ingredient -1 -2 6 3 8)
                               "Cinnamon" (ingredient 2 3 -2 -1 3))])
        (check-equal?
         (property-score permutation "capacity" ingredients) 68)
        (check-equal?
         (property-score permutation "durability" ingredients) 80)
        (check-equal?
         (property-score permutation "flavor" ingredients) 152)
        (check-equal?
         (property-score permutation "texture" ingredients) 76))

      (let ([ingredients (hash "Butterscotch" (ingredient -1 -2 6 3 8)
                               "Cinnamon" (ingredient 2 -3 -2 -1 3))])
        (check-equal?
         (property-score permutation "durability" ingredients) 0)))))

(define (cookie-score permutation ingredients)
  (let* ([totals (list (property-score permutation "capacity" ingredients)
                       (property-score permutation "durability" ingredients)
                       (property-score permutation "flavor" ingredients)
                       (property-score permutation "texture" ingredients))])
    (apply * totals)))
(module+ test
  (test-case "cookie-score"
    (check-equal?
     (cookie-score (list (list "Butterscotch" 44) (list "Cinnamon" 56))
                   (hash "Butterscotch" (ingredient -1 -2 6 3 8)
                         "Cinnamon" (ingredient 2 3 -2 -1 3)))
     62842880)))

(define parse-re #px"(\\w+): capacity ([-\\d]+), durability ([-\\d]+), flavor ([-\\d]+), texture ([-\\d]+), calories ([-\\d]+)")
(define (parse s)
  (let* ([l (regexp-match parse-re s)]
         [k (list-ref l 1)]
         [v (apply ingredient (map string->number (cddr l)))])
    (list k v)))
(module+ test
  (test-case "parse"
    (check-equal?
     (parse "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8")
     (list "Butterscotch" (ingredient -1 -2 6 3 8)))))

(define (winning-cookie-score xs)
  (let* ([ingredients (map parse xs)]
         [ingredients-names (map car ingredients)]
         [ingredients-hash (for/hash ([x ingredients]) (apply values x))]
         [score (λ (x) (cookie-score (for/list ([n x] [s ingredients-names]) (list s n))
                                     ingredients-hash))])
    (apply max (map score (cookie-permutations 100)))))
