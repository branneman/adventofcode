#lang racket/base

(provide app)

(require racket/match)
(module+ test (require rackunit))

(define (next-state-point curr neighbours)
  (cond
    [(and curr
          (or (= neighbours 2)
              (= neighbours 3)))]
    [(and (not curr)
          (= neighbours 3))]
    [else #f]))
(module+ test
  (test-case "next-state-point"
    (check-false (next-state-point #t 0))
    (check-false (next-state-point #t 1))
    (check-true (next-state-point #t 2))
    (check-true (next-state-point #t 3))
    (check-false (next-state-point #t 4))

    (check-false (next-state-point #f 0))
    (check-false (next-state-point #f 1))
    (check-false (next-state-point #f 2))
    (check-true (next-state-point #f 3))
    (check-false (next-state-point #f 4))))

(define (out-of-bounds? grid x y)
  (or (negative? (min x y))
      (>= (max x y) (vector-length grid))))
(module+ test
  (test-case "out-of-bounds?"
    (let ([test-grid (vector (vector #f #t #f #f #f #f)
                             (vector #f #f #f #f #f #f)
                             (vector #f #f #f #f #f #f)
                             (vector #f #f #f #f #f #f)
                             (vector #f #f #f #f #t #f)
                             (vector #f #f #f #f #f #f))])
      (check-false (out-of-bounds? test-grid 0 0))
      (check-false (out-of-bounds? test-grid 5 5))
      (check-true (out-of-bounds? test-grid -1 0))
      (check-true (out-of-bounds? test-grid 0 -1))
      (check-true (out-of-bounds? test-grid 5 6))
      (check-true (out-of-bounds? test-grid 6 5))
      (check-true (out-of-bounds? test-grid 6 6)))))

(define (sort-procedure? xs ys)
  (let recur ([x xs] [y ys])
    (cond
      [(or (null? x) (null? y))
       #f]
      [(not (equal? (car x) (car y)))
       (< (car x) (car y))]
      [else
       (recur (cdr x) (cdr y))])))
(module+ test
  (test-case "sort-procedure?"
    (check-false
     (sort-procedure? null  null))
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

(define (neighbours grid x y)
  (let ([points (list (list x (sub1 y))
                      (list (add1 x) (sub1 y))
                      (list (add1 x) y)
                      (list (add1 x) (add1 y))
                      (list x (add1 y))
                      (list (sub1 x) (add1 y))
                      (list (sub1 x) y)
                      (list (sub1 x) (sub1 y)))])
    (sort
     (filter (match-lambda [(list x y)
                            (and (not (out-of-bounds? grid x y))
                                 (equal? #t (vector-ref (vector-ref grid y) x)))])
             points)
     sort-procedure?)))
(module+ test
  (test-case "neighbours"
    (let ([test-grid (vector (vector #t #t #t #t #t #t)
                             (vector #t #t #t #t #t #t)
                             (vector #t #t #t #t #t #t)
                             (vector #t #t #t #t #t #t)
                             (vector #t #t #t #t #t #t)
                             (vector #t #t #t #t #t #t))])
      (check-equal?
       (neighbours test-grid 0 0)
       '((0 1) (1 0) (1 1)))
      (check-equal?
       (neighbours test-grid 1 0)
       '((0 0) (0 1) (1 1) (2 0) (2 1)))
      (check-equal?
       (neighbours test-grid 4 4)
       '((3 3) (3 4) (3 5) (4 3) (4 5) (5 3) (5 4) (5 5))))
    (let ([test-grid (vector (vector #f #t #f #f #f #f)
                             (vector #f #f #f #f #f #f)
                             (vector #f #f #f #f #f #f)
                             (vector #f #f #f #f #f #f)
                             (vector #f #f #f #f #t #f)
                             (vector #f #f #f #f #t #f))])
      (check-equal?
       (neighbours test-grid 0 0)
       '((1 0)))
      (check-equal?
       (neighbours test-grid 5 5)
       '((4 4) (4 5))))))

(define (next-state grid)
  (for/vector ([x-grid grid]
               [y (vector-length grid)])
    (for/vector ([value x-grid]
                 [x (vector-length x-grid)])
      (next-state-point value (length (neighbours grid x y))))))
(module+ test
  (test-case "next-state"
    (check-equal?
     (next-state (vector (vector #f #t #f #t #f #t)
                         (vector #f #f #f #t #t #f)
                         (vector #t #f #f #f #f #t)
                         (vector #f #f #t #f #f #f)
                         (vector #t #f #t #f #f #t)
                         (vector #t #t #t #t #f #f)))
     (vector (vector #f #f #t #t #f #f)
             (vector #f #f #t #t #f #t)
             (vector #f #f #f #t #t #f)
             (vector #f #f #f #f #f #f)
             (vector #t #f #f #f #f #f)
             (vector #t #f #t #t #f #f)))))

(define (parse-grid input-grid)
  (for/vector ([y input-grid])
    (for/vector ([x y])
      (eqv? x #\#))))
(module+ test
  (test-case "parse-grid"
    (check-equal?
     (parse-grid (list "..##.."
                       "..##.#"
                       "...##."
                       "......"
                       "#....."
                       "#.##.."))
     (vector (vector #f #f #t #t #f #f)
             (vector #f #f #t #t #f #t)
             (vector #f #f #f #t #t #f)
             (vector #f #f #f #f #f #f)
             (vector #t #f #f #f #f #f)
             (vector #t #f #t #t #f #f)))))

(define (count-true grid)
  (for/sum ([x-grid grid])
    (for/sum ([value x-grid])
      (if value 1 0))))
(module+ test
  (test-case "count-true"
    (check-equal?
     (count-true (vector (vector #f #t #f #t #f #t)
                         (vector #f #f #f #t #t #f)
                         (vector #t #f #f #f #f #t)
                         (vector #f #f #t #f #f #f)
                         (vector #t #f #t #f #f #t)
                         (vector #t #t #t #t #f #f)))
     15)))

(define (app input-lines)
  (let* ([input-grid (parse-grid input-lines)]
         [output-grid (for/fold ([grid input-grid])
                                ([i 100])
                        (next-state grid))])
    (count-true output-grid)))
