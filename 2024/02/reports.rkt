#lang racket/base

(provide
 app)
(require
  racket/string
  racket/function)
(module+ test
  (require rackunit))

(define (app input)
  (for/sum ([report (parse-into-lists input)])
    (if (is-safe-report? report) 1 0)))

(define (parse-into-lists str)
  (map (compose ((curry map) string->number)
                string-split)
       (string-split str "\n")))
(module+ test
  (test-case
   "parse-into-lists"
   (check-equal?
    (parse-into-lists "27 29 32 33\n57 60 61 62")
    '((27 29 32 33) (57 60 61 62)))))

(define (is-gradual-changes? xs)
  (let loop ([xs xs])
    (cond
      [(null? (cdr xs)) #t]
      [(is-gradual-change? (- (cadr xs) (car xs)))
       (loop (cdr xs))]
      [else #f])))
(module+ test
  (test-case
   "is-gradual-changes?"
   (check-true (is-gradual-changes? '(7 6 4 2 1)))
   (check-false (is-gradual-changes? '(1 2 7 8 9)))))

(define (is-gradual-change? n)
  (and (<= (abs n) 3)
       (not (zero? n))))
(module+ test
  (test-case
   "is-gradual-change?"
   (check-true (is-gradual-change? 1))
   (check-true (is-gradual-change? 2))
   (check-true (is-gradual-change? 3))
   (check-true (is-gradual-change? -1))
   (check-true (is-gradual-change? -2))
   (check-true (is-gradual-change? -3))
   (check-false (is-gradual-change? 0))
   (check-false (is-gradual-change? 4))
   (check-false (is-gradual-change? -4))))

(define (is-same-direction? xs)
  (or (equal? xs (sort xs <))
      (equal? xs (sort xs >))))
(module+ test
  (test-case
   "is-same-direction?"
   (check-true (is-same-direction? '(1 2 3)))
   (check-true (is-same-direction? '(3 2 1)))
   (check-false (is-same-direction? '(1 2 1)))
   (check-false (is-same-direction? '(1 2 3 2 1)))))

(define (is-safe-report? xs)
  (and (is-gradual-changes? xs)
       (is-same-direction? xs)))
(module+ test
  (test-case
   "is-safe-report?"
   (check-true (is-safe-report? '(7 6 4 2 1)))
   (check-false (is-safe-report? '(1 2 7 8 9)))
   (check-false (is-safe-report? '(9 7 6 2 1)))
   (check-false (is-safe-report? '(1 3 2 4 5)))
   (check-false (is-safe-report? '(8 6 4 4 1)))
   (check-true (is-safe-report? '(1 3 6 7 9)))))
