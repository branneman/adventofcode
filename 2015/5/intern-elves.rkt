#lang racket

(provide vowel?
         at-least-three-vowels?
         at-least-one-letter-twice-in-a-row?
         contains-some?
         nice-string?
         app)

(define vowels '(#\a #\e #\i #\o #\u))
(define disallowed-strings '("ab" "cd" "pq" "xy"))

(define (vowel? s)
  (not (false? (member s vowels))))

(define (at-least-three-vowels? s)
  (define (f xs count)
    (cond
      [(= count 3)
       #t]
      [(empty? xs)
       #f]
      [(vowel? (car xs))
       (f (cdr xs) (+ 1 count))]
      [else
       (f (cdr xs) count)]))
  (f (string->list s) 0))

(define (at-least-one-letter-twice-in-a-row? s)
  (define (f xs)
    (cond
      [(<= (length xs) 1)
       #f]
      [(equal? (car xs) (cadr xs))
       #t]
      [else
       (f (cdr xs))]))
  (f (string->list s)))

(define (contains-some? xs s)
  (define (f xs)
    (cond
      [(empty? xs)
       #f]
      [(string-contains? s (car xs))
       #t]
      [else
       (f (cdr xs))]))
  (f xs))

(define (nice-string? s)
  (and (at-least-three-vowels? s)
       (at-least-one-letter-twice-in-a-row? s)
       (not (contains-some? disallowed-strings s))))

(define (app xs)
  (length (filter (λ (x) (equal? #t x))
                  (map nice-string? xs))))
