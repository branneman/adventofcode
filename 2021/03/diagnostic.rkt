#lang racket/base

(provide
 app)
(module+ test
  (require rackunit))

(define (app xs)
  (define binary-numbers (flip-2d-list (parse xs)))
  (* (gamma-rate binary-numbers)
     (epsilon-rate binary-numbers)))
(module+ test
  (test-case "app"
    (check-equal?
     (app '("00100" "11110" "10110" "10111" "10101" "01111"
            "00111" "11100" "10000" "11001" "00010" "01010"))
     198)))

(define (gamma-rate xs)
  (binary->decimal (binary-list->number (map most-common-bit xs))))

(define (epsilon-rate xs)
  (binary->decimal (binary-list->number (map least-common-bit xs))))

(define (flip-2d-list xxs)
  (let ([x-len (length (car xxs))]
        [y-len (length xxs)])
    (for/list ([new-y x-len])
      (for/list ([new-x y-len])
        (list-ref (list-ref xxs new-x) new-y)))))
(module+ test
  (test-case "flip-2d-list"
    (check-equal?
     (flip-2d-list '((0 1 2)
                     (3 4 5)
                     (6 7 8)))
     '((0 3 6)
       (1 4 7)
       (2 5 8)))
    (check-equal?
     (flip-2d-list '((0 1 2)
                     (3 4 5)
                     (6 7 8)
                     (9 10 11)))
     '((0 3 6 9)
       (1 4 7 10)
       (2 5 8 11)))
    (check-equal?
     (flip-2d-list '((0 1 2 3)
                     (4 5 6 7)))
     '((0 4)
       (1 5)
       (2 6)
       (3 7)))))

(define (parse xs)
  (map (λ (s) (map (compose1 string->number string)
                   (string->list s)))
       xs))
(module+ test
  (test-case "parse"
    (check-equal?
     (parse '("00100" "11110" "10110"))
     '((0 0 1 0 0) (1 1 1 1 0) (1 0 1 1 0)))))

(define (most-common-bit xs)
  (let ([number-of-1s (apply + xs)]
        [l (/ (length xs) 2)])
    (if (> number-of-1s l) 1 0)))
(module+ test
  (test-case "most-common-bit"
    (check-equal?
     (most-common-bit '(0 1 1 1 1 0 0 1 1 1 0 0))
     1)
    (check-equal?
     (most-common-bit '(0 1 0 0 0 1 0 1 0 1 0 1))
     0)))

(define (least-common-bit xs)
  (if (= 0 (most-common-bit xs)) 1 0))
(module+ test
  (test-case "most-common-bit"
    (check-equal?
     (least-common-bit '(0 1 1 1 1 0 0 1 1 1 0 0))
     0)
    (check-equal?
     (least-common-bit '(0 1 0 0 0 1 0 1 0 1 0 1))
     1)))

(define (binary-list->number xs)
  (string->number (apply string-append (map number->string xs))))
(module+ test
  (test-case "binary-list->number"
    (check-equal?
     (binary-list->number '(0 0 1 1 0 0))
     1100)))

(define (binary->decimal n)
  (if (zero? n)
      n
      (+ (modulo n 10) (* 2 (binary->decimal (quotient n 10))))))
(module+ test
  (test-case "binary->decimal"
    (check-equal? (binary->decimal 0) 0)
    (check-equal? (binary->decimal 1) 1)
    (check-equal? (binary->decimal 10) 2)
    (check-equal? (binary->decimal 110) 6)
    (check-equal? (binary->decimal 10101) 21)
    (check-equal? (binary->decimal 1001011) 75)
    (check-equal? (binary->decimal 10001001) 137)))
