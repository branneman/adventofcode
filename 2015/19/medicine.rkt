#lang racket/base

(provide
 app)

(require
 racket/contract
 racket/string
 racket/dict
 racket/list
 racket/math
 racket/match)
(module+ test
  (require rackunit))

; like srfi/13 string-index, but returns list of indexes
(define/contract (string-indexes str contained)
  (string? string? . -> . (listof integer?))
  (reverse
   (for/fold ([acc '()])
             ([idx (string-length str)])
     (if (string-prefix? (substring str idx)
                         contained)
         (cons idx acc)
         acc))))
(module+ test
  (test-case "string-indexes"
    (check-equal? ; supports no results
     (string-indexes "abc" "def")
     '())
    (check-equal? ; supports multiple results
     (string-indexes "HOH" "H")
     '(0 2))
    (check-equal? ; supports overlapping patterns
     (string-indexes "xxyxyxyzz" "xyxy")
     '(1 3))))

(define/contract (string-replace-at str search replace idx)
  (string? string? string? (or/c zero? positive-integer?) . -> . string?)
  (let ([before (substring str 0 idx)]
        [after (substring str (+ idx (string-length search)))])
    (string-append before replace after)))
(module+ test
  (test-case "string-replace-at"
    (check-equal?
     (string-replace-at "foobarbaz" "bar" "rab" 3)
     "foorabbaz")
    (check-equal?
     (string-replace-at "aac" "a" "b" 1)
     "abc")
    (check-equal?
     (string-replace-at "aac" "a" "BBBB" 1)
     "aBBBBc")
    (check-equal?
     (string-replace-at "H2O" "H" "OO" 0)
     "OO2O")))

(define/contract (replace-rule rule str)
  (pair? string? . -> . (listof string?))
  (let ([search (symbol->string (car rule))]
        [replace (symbol->string (cdr rule))])
    (if (not (string-contains? str search))
        (list str)
        (for/list ([idx (string-indexes str search)])
          (string-replace-at str search replace idx)))))
(module+ test
  (test-case "replace-rule"
    (check-equal?
     (replace-rule '(O . HH) "HOH")
     '("HHHH"))
    (check-equal?
     (replace-rule '(H . HO) "HOH")
     '("HOOH" "HOHO"))
    (check-equal?
     (replace-rule '(H . OH) "HOH")
     '("OHOH" "HOOH"))))

(define/contract (replace-rules rules str)
  ((listof pair?) string? . -> . (listof string?))
  (foldl (λ (rule lst)
           (append lst (replace-rule rule str)))
         '()
         rules))
(module+ test
  (test-case "replace-rules"
    (check-equal?
     (replace-rules (list '(H . HO) '(H . OH) '(O . HH))
                    "HOH")
     '(; H => HO
       "HOOH" "HOHO"
       ; H => OH
       "OHOH" "HOOH"
       ; O => HH
       "HHHH"))

    (check-equal?
     (replace-rules (list '(H . HO) '(H . OH) '(O . HH))
                    "HOHOHO")
     '(; H => HO
       "HOOHOHO" "HOHOOHO" "HOHOHOO"
       ; H => OH
       "OHOHOHO" "HOOHOHO" "HOHOOHO"
       ; O => HH
       "HHHHOHO" "HOHHHHO" "HOHOHHH"))))

(define/contract (parse lines)
  ((listof string?) . -> . dict?)
  (map (λ (line)
         (match (string-split (string-trim line) " => ")
           [(list a b)
            (cons (string->symbol a) (string->symbol b))]))
       lines))
(module+ test
  (test-case "parse"
    (check-equal?
     (parse (list "Al => ThF" "Al => ThRnFAr"))
     (list '(Al . ThF) '(Al . ThRnFAr)))
    (check-equal?
     (parse (list "Al => ThF" "Al => ThRnFAr\n"))
     (list '(Al . ThF) '(Al . ThRnFAr)))))

(define/contract (app input-replacements molecule)
  ((listof string?) string? . -> . integer?)
  (length
   (remove-duplicates
    (replace-rules (parse input-replacements) molecule))))
