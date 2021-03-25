#lang racket

(provide string-count-char-occurences
         not-quite-lisp-part1
         not-quite-lisp-part2)

(define (string-count-char-occurences str char)
  (define (f occurences str char)
    (if (equal? 0 (string-length str))
        occurences
        (if (equal? char (string-ref str 0))
            (f (+ 1 occurences) (substring str 1) char)
            (f occurences (substring str 1) char))
        ))
  (f 0 str char))

(define UP #\()
(define DOWN #\))

; what floor do the instructions take you?
(define (not-quite-lisp-part1 str)
  (+ (string-count-char-occurences str UP)
     (- (string-count-char-occurences str DOWN))))

; find the position of the first character which causes you to enter floor -1
(define (not-quite-lisp-part2 str)
  (define (f position level str)
    (cond
      [(< level 0)
       (- position 1)]
      [(equal? 0 (string-length str))
       #f]
      [(equal? UP (string-ref str 0))
       (f (+ 1 position) (+ level 1) (substring str 1))]
      [(equal? DOWN (string-ref str 0))
       (f (+ 1 position) (- level 1) (substring str 1))]
      ))
  (f 1 0 str))
