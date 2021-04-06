#lang racket

(require "wires.rkt")

(define input-file
  (let* ([from (find-system-path 'orig-dir)]
         [to (find-system-path 'run-file)]
         [dir (path-only (find-relative-path from to))]
         [base (if (false? dir) 'same dir)])
    (build-path base "input")))

(define lines (file->lines input-file))

(display "signal at a (part 1): ")
(hash-ref (app-part1 lines) "a")

(display "signal at a (part 2): ")
(hash-ref (app-part2 lines) "a")
