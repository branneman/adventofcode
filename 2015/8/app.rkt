#lang racket

(require "matchsticks.rkt")

(define input-file
  (let* ([from (find-system-path 'orig-dir)]
         [to (find-system-path 'run-file)]
         [dir (path-only (find-relative-path from to))]
         [base (if (false? dir) 'same dir)])
    (build-path base "input")))

(define lines (file->lines input-file))

(display "number (part 1): ")
(app-part1 lines)

(display "number (part 2): ")
(app-part2 lines)
