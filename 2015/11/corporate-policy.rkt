#lang racket

(provide next-password)

(require continued-fractions/bases)
(module+ test (require rackunit))

(define (next-password s)
  (let recur ([str (increment-string s)])
    (cond
      [(is-valid? str) str]
      [else (recur (increment-string str))])))
(module+ test
  (check-equal? (next-password "abcdfezz") "abcdffaa")
  (check-equal? (next-password "ghijklmn") "ghjaabcc")
  (check-equal? (next-password "cqjxjnds") "cqjxxyzz"))

(define (increment-string s)
  (let ([len (string-length s)]
        [str (base10->base26 (add1 (base26->base10 s)))])
    (if (= len (string-length str))
        str
        (string-append (make-string (- len (string-length str)) #\a) str))))
(module+ test
  (check-equal? (increment-string "ab") "ac")
  (check-equal? (increment-string "aaaab") "aaaac")
  (check-equal? (increment-string "xx") "xy")
  (check-equal? (increment-string "xy") "xz")
  (check-equal? (increment-string "xz") "ya")
  (check-equal? (increment-string "ya") "yb"))

(define base26 (make-representation #:terms "abcdefghijklmnopqrstuvwxyz"))
(define (base10->base26 n)
  (parameterize ([representation base26])
    (->string n)))
(define (base26->base10 s)
  (parameterize ([representation base26])
    (->number s)))
(module+ test
  (check-equal? (base10->base26 0) "a")
  (check-equal? (base10->base26 25) "z")
  (check-equal? (base10->base26 26) "ba")
  (check-equal? (base10->base26 1209) "bun")
  (check-equal? (base26->base10 "a") 0)
  (check-equal? (base26->base10 "aaaaa") 0)
  (check-equal? (base26->base10 "c") 2)
  (check-equal? (base26->base10 "z") 25)
  (check-equal? (base26->base10 "ba") 26)
  (check-equal? (base26->base10 "ac") 2)
  (check-equal? (base26->base10 "bun") 1209)
  (check-equal? (base26->base10 (base10->base26 0)) 0)
  (check-equal? (base26->base10 (base10->base26 42)) 42))

(define (is-valid? s)
  (and (has-straight? s)
       (not (has-forbidden-chars? s))
       (has-two-non-overlapping-pairs? s)))
(module+ test
  (check-equal? (is-valid? "hijklmmn") #f) ; has-forbidden-chars
  (check-equal? (is-valid? "abbceffg") #f) ; not has-straight
  (check-equal? (is-valid? "abbcegjk") #f) ; not has-two-non-overlapping-pairs
  (check-equal? (is-valid? "abcdffaa") #t)
  (check-equal? (is-valid? "ghjaabcc") #t))

(define (has-straight? s)
  (cond
    [(< (string-length s) 3)
     #f]
    [(and (not (equal? #\y (string-ref s 0)))
          (not (equal? #\z (string-ref s 0)))
          (equal? (string-ref s 1) (next-letter (string-ref s 0)))
          (equal? (string-ref s 2) (next-letter (string-ref s 1))))
     #t]
    [else
     (has-straight? (substring s 1 (string-length s)))]))
(module+ test
  (check-equal? (has-straight? "") #f)
  (check-equal? (has-straight? "a") #f)
  (check-equal? (has-straight? "ab") #f)
  (check-equal? (has-straight? "abd") #f)
  (check-equal? (has-straight? "abc") #t)
  (check-equal? (has-straight? "xyz") #t)
  (check-equal? (has-straight? "yza") #f)
  (check-equal? (has-straight? "zab") #f)
  (check-equal? (has-straight? "aaabccc") #t)
  (check-equal? (has-straight? "aabbcc") #f)
  (check-equal? (has-straight? "hijklmmn") #t)
  (check-equal? (has-straight? "abcdefgh") #t)
  (check-equal? (has-straight? "ghijklmn") #t)
  (check-equal? (has-straight? "cqjxxyzz") #t))

(define (next-letter c)
  (let* ([curr (char->integer c)]
         [next (if (= 122 curr) 97 (add1 curr))])
    (integer->char next)))
(module+ test
  (check-equal? (next-letter #\a) #\b)
  (check-equal? (next-letter #\b) #\c)
  (check-equal? (next-letter #\y) #\z)
  (check-equal? (next-letter #\z) #\a))

(define (has-forbidden-chars? s)
  (ormap (λ (x) (string-contains? s x)) '("i" "o" "l")))
(module+ test
  (check-equal? (has-forbidden-chars? "abc") #f)
  (check-equal? (has-forbidden-chars? "fghij") #t)
  (check-equal? (has-forbidden-chars? "aiaoala") #t))

(define (has-two-non-overlapping-pairs? s)
  (define (f xs c)
    (cond
      [(= 2 c)
       #t]
      [(< (length xs) 2)
       #f]
      [(equal? (car xs) (cadr xs))
       (f (cddr xs) (add1 c))]
      [else
       (f (cdr xs) c)]))
  (f (string->list s) 0))
(module+ test
  (check-equal? (has-two-non-overlapping-pairs? "") #f)
  (check-equal? (has-two-non-overlapping-pairs? "a") #f)
  (check-equal? (has-two-non-overlapping-pairs? "bb") #f)
  (check-equal? (has-two-non-overlapping-pairs? "aaa") #f)
  (check-equal? (has-two-non-overlapping-pairs? "abcxyz") #f)
  (check-equal? (has-two-non-overlapping-pairs? "abcxxyz") #f)
  (check-equal? (has-two-non-overlapping-pairs? "xyxy") #f)
  (check-equal? (has-two-non-overlapping-pairs? "aaxaa") #t)
  (check-equal? (has-two-non-overlapping-pairs? "axybxyc") #f)
  (check-equal? (has-two-non-overlapping-pairs? "aershierz") #f)
  (check-equal? (has-two-non-overlapping-pairs? "aabcdefgaa") #t)
  (check-equal? (has-two-non-overlapping-pairs? "abcdefgh") #f)
  (check-equal? (has-two-non-overlapping-pairs? "abcdffaa") #t)
  (check-equal? (has-two-non-overlapping-pairs? "ghjaabcc") #t))
