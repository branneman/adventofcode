#lang racket/base

(require
  racket/path
  racket/file
  "distance.rkt")

(define input-file
  (let* ([from (find-system-path 'orig-dir)]
         [to (find-system-path 'run-file)]
         [dir (path-only (find-relative-path from to))]
         [base (if (not dir) 'same dir)])
    (build-path base "input")))

(define input (file->string input-file))

(display "total dinstance between the two lists: ")
(displayln (app input))
