#lang racket

(provide str-length
         str-length-in-memory
         str-length-code-representation
         app-part1
         app-part2)

(define (str-length s)
  (string-length s))

(define (str-length-in-memory s)
  (string-length (read (open-input-string s))))

(define (str-length-code-representation s)
  (let ([o (open-output-string)])
    (print s o)
    (string-length (get-output-string o))))

(define (app-part1 xs)
  (foldl (λ (s acc)
           (+ acc (- (str-length s)
                     (str-length-in-memory s))))
         0
         xs))

(define (app-part2 xs)
  (foldl (λ (s acc)
           (+ acc (- (str-length-code-representation s)
                     (str-length s))))
         0
         xs))
