#lang racket

(provide string-count-char-occurences
         not-quite-lisp)

(define (string-count-char-occurences str char)
  (define (f occurences str char)
    (if (equal? 0 (string-length str))
        occurences
        (if (equal? char (string-ref str 0))
            (f (+ 1 occurences) (substring str 1) char)
            (f occurences (substring str 1) char))
        ))
  (f 0 str char))

(define (not-quite-lisp str)
  (+ (string-count-char-occurences str #\()
     (- (string-count-char-occurences str #\)))))
