#lang racket/base

(provide
 app)
(require
  racket/string)
(module+ test
  (require rackunit))

(define (app input)
  (define grid (parse-into-lists input))
  (for*/sum ([X-coords (find-all-Xes grid)]
             [possible-coords (all-directions-coords grid X-coords)])
    (if (is-valid-XMAS possible-coords (eq-at-coords? grid)) 1 0)))
(module+ test
  (test-case
   "app"
   (check-equal?
    18
    (app "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX\n"))))

(define (parse-into-lists str)
  (map string->list
       (string-split str "\n")))
(module+ test
  (test-case
   "parse-into-lists"
   (check-equal?
    (parse-into-lists "MMMSXXMASM\nMSAMXMSMSA\n")
    '((#\M #\M #\M #\S #\X #\X #\M #\A #\S #\M)
      (#\M #\S #\A #\M #\X #\M #\S #\M #\S #\A)))))

(define (find-all-Xes grid)
  (for*/list ([y (in-range (length grid))]
              [x (in-range (length (list-ref grid y)))]
              #:when (eq? (list-ref (list-ref grid y) x) #\X))
    (list y x)))
(module+ test
  (test-case
   "find-all-Xes"
   (check-equal?
    (find-all-Xes '((#\M #\M #\M #\S #\X #\X #\M #\A #\S #\M)
                    (#\M #\S #\A #\M #\X #\M #\S #\M #\S #\A)))
    '((0 4) (0 5) (1 4)))))

(define (all-directions-coords grid coords)
  (let ([y (car coords)]
        [x (cadr coords)])
    (filter
     (λ (x) (in-bounds? grid x))
     (list
      (list (list y x) (list y (+ x 1)) (list y (+ x 2)) (list y (+ x 3))) ; horizontal to right
      (list (list y x) (list y (- x 1)) (list y (- x 2)) (list y (- x 3))) ; horizontal to left
      (list (list y x) (list (+ y 1) x) (list (+ y 2) x) (list (+ y 3) x)) ; vertical to bottom
      (list (list y x) (list (- y 1) x) (list (- y 2) x) (list (- y 3) x)) ; vertical to top
      (list (list y x) (list (- y 1) (+ x 1)) (list (- y 2) (+ x 2)) (list (- y 3) (+ x 3))) ; diagonal to top-right
      (list (list y x) (list (+ y 1) (+ x 1)) (list (+ y 2) (+ x 2)) (list (+ y 3) (+ x 3))) ; diagonal to bottom-right
      (list (list y x) (list (+ y 1) (- x 1)) (list (+ y 2) (- x 2)) (list (+ y 3) (- x 3))) ; diagonal to bottom-left
      (list (list y x) (list (- y 1) (- x 1)) (list (- y 2) (- x 2)) (list (- y 3) (- x 3))) ; diagonal to top-left
      ))))
(module+ test
  (define test-grid-1 '((#\M #\M #\M #\S #\X #\X #\M #\A #\S #\M)
                        (#\M #\S #\A #\M #\X #\M #\S #\M #\S #\A)
                        (#\A #\M #\X #\S #\X #\M #\A #\A #\M #\M)
                        (#\M #\S #\A #\M #\A #\S #\M #\S #\M #\X)
                        (#\X #\M #\A #\S #\A #\M #\X #\A #\M #\M)
                        (#\X #\X #\A #\M #\M #\X #\X #\A #\M #\A)
                        (#\S #\M #\S #\M #\S #\A #\S #\X #\S #\S)
                        (#\S #\A #\X #\A #\M #\A #\S #\A #\A #\A)
                        (#\M #\A #\M #\M #\M #\X #\M #\M #\M #\M)
                        (#\M #\X #\M #\X #\A #\X #\M #\A #\S #\X)))
  (test-case
   "all-directions-coords"
   (check-equal?
    (all-directions-coords test-grid-1 '(5 5))
    '(
      ((5 5) (5 6) (5 7) (5 8)) ; horizontal to right
      ((5 5) (5 4) (5 3) (5 2)) ; horizontal to left
      ((5 5) (6 5) (7 5) (8 5)) ; vertical to bottom
      ((5 5) (4 5) (3 5) (2 5)) ; vertical to top
      ((5 5) (4 6) (3 7) (2 8)) ; diagonal to top-right
      ((5 5) (6 6) (7 7) (8 8)) ; diagonal to bottom-right
      ((5 5) (6 4) (7 3) (8 2)) ; diagonal to bottom-left
      ((5 5) (4 4) (3 3) (2 2)) ; diagonal to top-left
      ))))

(define (in-bounds? grid list-of-coords)
  (define max-y (sub1 (length grid)))
  (define max-x (sub1 (length (car grid))))
  (andmap (λ (coords) (and (>= (car coords) 0)
                           (>= (cadr coords) 0)
                           (<= (car coords) max-y)
                           (<= (cadr coords) max-x)))
          list-of-coords))
(module+ test
  (define test-grid-2 '((#\M #\M #\M #\S #\X #\X #\M #\A #\S #\M)
                        (#\M #\S #\A #\M #\X #\M #\S #\M #\S #\A)))
  (test-case
   "in-bounds?"
   (check-true (in-bounds? test-grid-2 '((0 5) (0 6) (0 7) (0 8))))
   (check-false (in-bounds? test-grid-2 '((0 5) (1 5) (2 5) (-3 5))))
   (check-false (in-bounds? test-grid-2 '((0 5) (1 -4) (2 3) (3 2))))
   (check-false (in-bounds? test-grid-2 '((0 5) (1 4) (2 3) (3 2))))))

(define (is-valid-XMAS list-of-coords eq-at-coords?)
  (and
   (eq-at-coords? (list-ref list-of-coords 0) #\X)
   (eq-at-coords? (list-ref list-of-coords 1) #\M)
   (eq-at-coords? (list-ref list-of-coords 2) #\A)
   (eq-at-coords? (list-ref list-of-coords 3) #\S)
   #t))
(module+ test
  (test-case
   "is-valid-XMAS"
   (check-true
    (is-valid-XMAS '((5 5) (5 6) (5 7) (5 8)) (λ _ #t)))
   (check-false
    (is-valid-XMAS '((5 5) (6 6) (7 7) (8 8)) (λ _ #f)))))

(define (eq-at-coords? grid)
  (λ (coords chr)
    (let ([y (car coords)]
          [x (cadr coords)])
      (eq? (list-ref (list-ref grid y) x) chr))))
(module+ test
  (define curried-eq-at-coords?
    (eq-at-coords? '((#\M #\M #\M #\S #\X #\X #\M #\A #\S #\M)
                     (#\M #\S #\A #\M #\X #\M #\S #\M #\S #\A))))
  (test-case
   "eq-at-coords?"
   (check-true
    (curried-eq-at-coords? '(0 5) #\X))
   (check-true
    (curried-eq-at-coords? '(0 6) #\M))
   (check-true
    (curried-eq-at-coords? '(0 7) #\A))
   (check-true
    (curried-eq-at-coords? '(0 8) #\S))
   (check-false
    (curried-eq-at-coords? '(0 1) #\X))))
