#lang racket

(require "intern-elves.rkt")

(define input-file
  (let* ([from (find-system-path 'orig-dir)]
         [to (find-system-path 'run-file)]
         [dir (path-only (find-relative-path from to))]
         [base (if (false? dir) 'same dir)])
    (build-path base "input")))

(define lines (file->lines input-file))

(display "number of nice strings, part 1: ")
(app nice-string-part1? lines)

(display "number of nice strings, part 2: ")
(app nice-string-part2? lines)
