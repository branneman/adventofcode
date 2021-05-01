#lang racket

(provide winning-cookie-score
         winning-cookie-500-calories-score)

(module+ test (require rackunit))

(struct ingredient (capacity durability flavor texture calories) #:transparent)
(define (property->accessor s)
  (hash-ref (hash "capacity" ingredient-capacity
                  "durability" ingredient-durability
                  "flavor" ingredient-flavor
                  "texture" ingredient-texture
                  "calories" ingredient-calories)
            s))

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
    (sort lst cookie-sort-procedure)))
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

(define (cookie-sort-procedure x y)
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
     (< (car x) (car y))]))
(module+ test
  (test-case "cookie-sort"
    (check-equal?
     (sort '((1 2 3) (2 3 1) (3 1 2)) cookie-sort-procedure)
     '((1 2 3) (2 3 1) (3 1 2)))

    (check-equal?
     (sort '((4 3 2) (2 3 10) (3 1 2)) cookie-sort-procedure)
     '((2 3 10) (3 1 2) (4 3 2)))

    (check-equal?
     (sort '((4 3 2) (2 3 10) (2 3 2)) cookie-sort-procedure)
     '((2 3 2) (2 3 10) (4 3 2)))

    (check-equal?
     (sort '((2 1 1 1) (1 2 1 1) (1 1 2 1) (1 1 1 2)) cookie-sort-procedure)
     '((1 1 1 2) (1 1 2 1) (1 2 1 1) (2 1 1 1)))))

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

(define (cookie-score permutation ingredients-names ingredients-hash)
  (let* ([p (for/list ([n permutation] [s ingredients-names]) (list s n))]
         [totals (list (property-score p "capacity" ingredients-hash)
                       (property-score p "durability" ingredients-hash)
                       (property-score p "flavor" ingredients-hash)
                       (property-score p "texture" ingredients-hash))])
    (apply * totals)))
(module+ test
  (test-case "cookie-score"
    (check-equal?
     (cookie-score (list 44 56)
                   (list "Butterscotch" "Cinnamon")
                   (hash "Butterscotch" (ingredient -1 -2 6 3 8)
                         "Cinnamon" (ingredient 2 3 -2 -1 3)))
     62842880)))

(define (calories-score permutation ingredients-names ingredients-hash)
  (property-score (for/list ([n permutation] [s ingredients-names]) (list s n))
                  "calories"
                  ingredients-hash))
(module+ test
  (test-case "calories-score"
    (check-equal?
     (calories-score (list 40 60)
                     (list "Butterscotch" "Cinnamon")
                     (hash "Butterscotch" (ingredient -1 -2 6 3 8)
                           "Cinnamon" (ingredient 2 3 -2 -1 3)))
     500)))

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

(define (ingredients xs)
  (let ([ingredients (map parse xs)])
    (values (map car ingredients)
            (for/hash ([x ingredients]) (apply values x)))))

(define (winning-cookie-score xs)
  (define-values (ingredients-names ingredients-hash) (ingredients xs))
  (apply max (map (λ (permutation) (cookie-score permutation ingredients-names ingredients-hash))
                  (cookie-permutations 100))))

(define (winning-cookie-500-calories-score xs)
  (define-values (ingredients-names ingredients-hash) (ingredients xs))
  (for/fold ([acc 0])
            ([permutation (cookie-permutations 100)])
    (cond
      [(= 500 (calories-score permutation ingredients-names ingredients-hash))
       (let ([score (cookie-score permutation ingredients-names ingredients-hash)])
         (if (> score acc) score acc))]
      [else acc])))
