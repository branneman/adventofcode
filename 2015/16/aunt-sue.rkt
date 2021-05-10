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

(define (match? x)
  (andmap (λ (y) (findf (λ (z) (equal? y z))
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
    (check-not-false
     (match? (list 1 (hash 'cats 7
                           'children 3
                           'goldfish 5))))
    ))

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
