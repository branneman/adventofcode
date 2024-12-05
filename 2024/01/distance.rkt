#lang racket/base

(provide
 app)
(require
  racket/string
  racket/list)
(module+ test
  (require rackunit))

(define (parse-into-lists str)
  (let* ([lines (string-split str "\n")]
         [list1 (sort (lines-to-list 0 lines) <)]
         [list2 (sort (lines-to-list 1 lines) <)]
         [pairs (zip list1 list2)])
    pairs))

(define (lines-to-list n lines)
  (map (λ (line)
         (string->number (list-ref (string-split line) n)))
       lines))
(module+ test
  (test-case
   "lines-to-list"
   (check-equal?
    (lines-to-list 0 (list "1 4" "2 5" "3 6"))
    (list 1 2 3))
   (check-equal?
    (lines-to-list 1 (list "1 4" "2 5" "3 6"))
    (list 4 5 6))))

(define (zip a b)
  (if (or (empty? a) (empty? b))
      '()
      (cons (list (car a) (car b))
            (zip (cdr a) (cdr b)))))
(module+ test
  (test-case
   "zip"
   (check-equal?
    (zip (list 1 2 3) (list 4 5 6))
    (list (list 1 4) (list 2 5) (list 3 6)))))

(define (distance pair)
  (abs (- (first pair) (second pair))))
(module+ test
  (test-case
   "distance"
   (check-equal? (distance (list 1 4)) 3)
   (check-equal? (distance (list 5 2)) 3)
   (check-equal? (distance (list 3 6)) 3)))

(define app
  (compose
   (λ (distances) (foldl + 0 distances))
   (λ (pairs) (map distance pairs))
   parse-into-lists))
