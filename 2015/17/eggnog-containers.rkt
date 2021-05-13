#lang racket

(provide container-combinations
         minimum-containers-combinations)

(module+ test (require rackunit))

(define (container-combinations total containers)
  (sort (filter (λ (c) (= total (apply + c)))
                (combinations containers))
        sort-procedure?))
(module+ test
  (test-case "container-combinations"
    (check-equal?
     (container-combinations 25 '(5 5 10 15 20))
     '((5 5 15)
       (5 20)
       (5 20)
       (10 15)))))

(define (minimum-containers-combinations total containers)
  (sort (foldl (λ (c acc)
                 (cond
                   [(not (= (apply + c) total))
                    acc]
                   [(empty? acc)
                    (list c)]
                   [(= (length c) (length (car acc)))
                    (cons c acc)]
                   [(< (length c) (length (car acc)))
                    (list c)]
                   [else acc]))
               empty
               (combinations containers))
        sort-procedure?))
(module+ test
  (test-case "minimum-containers-combinations"
    (check-equal?
     (minimum-containers-combinations 25 '(5 5 10 15 20))
     '((5 20)
       (5 20)
       (10 15)))))

(define (sort-procedure? xs ys)
  (let recur ([x xs] [y ys])
    (cond
      [(or (empty? x) (empty? y))
       #f]
      [(not (equal? (car x) (car y)))
       (< (car x) (car y))]
      [else
       (recur (cdr x) (cdr y))])))
(module+ test
  (test-case "sort-procedure?"
    (check-false
     (sort-procedure? empty empty))
    (check-true
     (sort-procedure? '(1) '(2)))
    (check-false
     (sort-procedure? '(3) '(2)))
    (check-true
     (sort-procedure? '(1 2 3) '(4 5 6)))
    (check-false
     (sort-procedure? '(5 2 3) '(4 5 6)))
    (check-true
     (sort-procedure? '(5 2 3) '(5 2 6)))
    (check-false
     (sort-procedure? '(5 2 3) '(5 2 2)))

    (check-equal?
     (sort '((1 2 3) (2 3 1) (3 1 2)) sort-procedure?)
     '((1 2 3) (2 3 1) (3 1 2)))
    (check-equal?
     (sort '((4 3 2) (2 3 10) (3 1 2)) sort-procedure?)
     '((2 3 10) (3 1 2) (4 3 2)))
    (check-equal?
     (sort '((4 3 2) (2 3 10) (2 3 2)) sort-procedure?)
     '((2 3 2) (2 3 10) (4 3 2)))
    (check-equal?
     (sort '((2 1 1 1) (1 2 1 1) (1 1 2 1) (1 1 1 2)) sort-procedure?)
     '((1 1 1 2) (1 1 2 1) (1 2 1 1) (2 1 1 1)))

    (check-equal?
     (sort '((1 2 3 4 5) (3 2 4 1 5) (3 2 4 2 5)) sort-procedure?)
     '((1 2 3 4 5) (3 2 4 1 5) (3 2 4 2 5)))))
