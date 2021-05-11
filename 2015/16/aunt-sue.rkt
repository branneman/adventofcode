#lang racket

(provide match-sue)

(module+ test (require rackunit))

(define ticker-tape  
  (hash 'children 3
        'cats 7
        'samoyeds 2
        'pomeranians 3
        'akitas 0
        'vizslas 0
        'goldfish 5
        'trees 3
        'cars 2
        'perfumes 1))

(define (match-prop? tt-prop match-prop)
  (cond
    [(or (and (equal? 'cats (car tt-prop))
              (equal? 'cats (car match-prop)))
         (and (equal? 'trees (car tt-prop))
              (equal? 'trees (car match-prop))))
     (> (cdr match-prop) (cdr tt-prop))]

    [(or (and (equal? 'pomeranians (car tt-prop))
              (equal? 'pomeranians (car match-prop)))
         (and (equal? 'goldfish (car tt-prop))
              (equal? 'goldfish (car match-prop))))
     (< (cdr match-prop) (cdr tt-prop))]

    [else
     (equal? match-prop tt-prop)]))
(module+ test
  (test-case "match-prop?"
    (check-false
     (match-prop? '(cars 3) '(cats 3)))
    (check-not-false
     (match-prop? '(cars 3) '(cars 3)))
    (check-false
     (match-prop? '(cats 3) '(cars 2)))
    (check-false
     (match-prop? '(cats 3) '(cars 4)))))

(define (match? x)
  (andmap (λ (match-prop)
            (findf (λ (tt-prop) (match-prop? tt-prop match-prop))
                   (hash->list ticker-tape)))
          (hash->list (cadr x))))
(module+ test
  (test-case "match?"
    (check-false
     (match? (list 1 (hash 'cars 9
                           'akitas 3
                           'goldfish 0))))
    (check-false
     (match? (list 1 (hash 'cars 9
                           'children 3
                           'goldfish 0))))
    (check-false
     (match? (list 1 (hash 'cars 7
                           'children 3
                           'goldfish 0))))
    (check-false
     (match? (list 1 (hash 'cats 7
                           'children 3
                           'goldfish 5))))
    (check-not-false
     (match? (list 1 (hash 'cats 8
                           'children 3
                           'goldfish 4))))))

(define (parse x)
  (let* ([l (cdr (regexp-match #px"Sue (\\d+): (.+)" x))]
         [number (string->number (car l))]
         [pairs (map (λ (s) (string-split s ": "))
                     (string-split (cadr l) ", "))]
         [props (map (λ (pair) (list (string->symbol (car pair))
                                     (string->number (cadr pair))))
                     pairs)])
    (list number (apply hash (flatten props)))))
(module+ test
  (test-case "parse"
    (check-equal?
     (parse "Sue 1: cars: 9, akitas: 3, goldfish: 0")
     (list 1 (hash 'cars 9
                   'akitas 3
                   'goldfish 0)))))

(define (match-sue xs)
  (let ([sue (for/list ([s xs])
               (let ([x (parse s)])
                 (if (match? x)
                     (car x)
                     #f)))])
    (car (filter identity sue))))
