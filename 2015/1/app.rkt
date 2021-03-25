#lang racket

(require "not-quite-lisp.rkt")

(define input-file
  (let* ([from (find-system-path 'orig-dir)]
         [to (find-system-path 'run-file)]
         [dir (path-only (find-relative-path from to))]
         [base (if (false? dir) 'same dir)])
    (build-path base "input")))

(define str (file->string input-file))

(display "final floor: ")
(not-quite-lisp-part1 str)
(newline)

(display "basement position: ")
(not-quite-lisp-part2 str)
(newline)
