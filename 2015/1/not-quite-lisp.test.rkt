(module not-quite-lisp racket
  (require rackunit
           "not-quite-lisp.rkt")

  (module+ test
    (test-case "string-count-char-occurences"
      (check-equal?
       1 (string-count-char-occurences "abbccc" #\a))
      (check-equal?
       2 (string-count-char-occurences "abbccc" #\b))
      (check-equal?
       3 (string-count-char-occurences "abbccc" #\c)))

    (test-case "not-quite-lisp"
      (check-equal?
       0 (not-quite-lisp "(())"))
      (check-equal?
       0 (not-quite-lisp "()()"))
      (check-equal?
       3 (not-quite-lisp "((("))
      (check-equal?
       3 (not-quite-lisp "(()(()("))
      (check-equal?
       3 (not-quite-lisp "))((((("))
      (check-equal?
       -1 (not-quite-lisp "())"))
      (check-equal?
       -1 (not-quite-lisp "))("))
      (check-equal?
       -3 (not-quite-lisp ")))"))
      (check-equal?
       -3 (not-quite-lisp ")())())")))))
