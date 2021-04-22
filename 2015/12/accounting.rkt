#lang racket

(provide total)

(require json)
(module+ test (require rackunit))

(define (total data)
  (let ([n 0])
    (walk (λ (x) (when (number? x) (set! n (+ n x))))
          data)
    n))
(module+ test
  (define test-json-1
    (read-json
     (open-input-string
      "{\"a\":[120,169,\"green\"],\"b\":{\"13\":\"37\",\"c\":42}}")))
  (check-equal? (total test-json-1) 331))

(define (walk f val)
  (cond
    [(list? val)
     (for ([x val])
       (walk f x))]
    [(hash? val)
     (when (not (member "red" (hash-values val)))
       (for ([(_ x) val])
         (walk f x)))]
    [else
     (f val)
     (void)]))
(module+ test
  (define test-json-2
    (read-json
     (open-input-string
      "{\"a\":[120,169,\"green\",\"red\"],\"b\":{\"blue\":\"orange\"}}")))
  (define (has-been-called-with input expected)
    (let ([xs empty])
      (walk (λ (x) (set! xs (cons x xs))) input)
      (check-equal? (reverse xs) expected)))
  (has-been-called-with 42 (list 42))
  (has-been-called-with "foo" (list "foo"))
  (has-been-called-with (list 13 37) (list 13 37))
  (has-been-called-with (hash "foo" "bar") (list "bar"))
  (has-been-called-with test-json-2 (list "orange" 120 169 "green" "red")))
