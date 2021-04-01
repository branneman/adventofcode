#lang racket

(provide vowel?
         at-least-three-vowels?
         at-least-one-letter-twice-in-a-row?
         contains-some?
         pair-exists?
         pair-appears-at-least-twice?
         at-least-one-repeating-letter-with-letter-in-between?
         nice-string-part1?
         nice-string-part2?
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

(define (pair-exists? xs pair)
  (cond
    [(< (length xs) 2)
     #f]
    [(equal? pair (list (car xs) (cadr xs)))
     #t]
    [else
     (pair-exists? (cdr xs) pair)]))

(define (pair-appears-at-least-twice? s)
  (define (f xs)
    (cond
      [(< (length xs) 4)
       #f]
      [(pair-exists? (cddr xs) (list (car xs) (cadr xs)))
       #t]
      [else
       (f (cdr xs))]))
  (f (string->list s)))

(define (at-least-one-repeating-letter-with-letter-in-between? s)
  (define (f xs)
    (cond
      [(< (length xs) 3)
       #f]
      [(and (equal? (car xs) (caddr xs)))
       #t]
      [else
       (f (cdr xs))]))
  (f (string->list s)))

(define (nice-string-part1? s)
  (and (at-least-three-vowels? s)
       (at-least-one-letter-twice-in-a-row? s)
       (not (contains-some? disallowed-strings s))))

(define (nice-string-part2? s)
  (and (pair-appears-at-least-twice? s)
       (at-least-one-repeating-letter-with-letter-in-between? s)))

(define (app f xs)
  (foldl (λ (curr acc) (if (f curr) (+ 1 acc) acc))
         0
         xs))
