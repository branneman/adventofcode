#lang racket

(provide parse
         unique-locations
         distance-path
         distance-route
         distances
         app)

(define (parse s)
  (let* ([l (regexp-match #px"(\\w+) to (\\w+) = (\\d+)" s)]
         [loc1 (list-ref l 1)]
         [loc2 (list-ref l 2)]
         [dist (string->number (list-ref l 3))])
    (append (sort (list loc1 loc2) string<?)
            (list dist))))

(define (unique-locations xs)
  (let ([l (foldl (λ (x acc) (append (take x 2) acc))
                  empty
                  xs)])
    (sort (remove-duplicates l) string<?)))

(define (distance-path loc1 loc2 xs)
  (let* ([locs (sort (list loc1 loc2) string<?)]
         [x (findf (λ (x) (and (equal? (car locs) (car x))
                               (equal? (cadr locs) (cadr x))))
                   xs)])
    (caddr x)))

(define (distance-route route xs)
  (cond
    [(= 2 (length route))
     (distance-path (car route) (cadr route) xs)]
    [else
     (+ (distance-path (car route) (cadr route) xs)
        (distance-route (cdr route) xs))]))

(define (distances xs)
  (map (λ (route) (distance-route route xs))
       (permutations (unique-locations xs))))

(define (app xs)
  (car (sort (distances (map parse xs)) <)))
