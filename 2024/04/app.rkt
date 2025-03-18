#lang racket/base

(require
  racket/path
  racket/file
  "search.rkt")

(define input-file
  (let* ([from (find-system-path 'orig-dir)]
         [to (find-system-path 'run-file)]
         [dir (path-only (find-relative-path from to))]
         [base (if (not dir) 'same dir)])
    (build-path base "input")))

(define input (file->string input-file))

(display "number of appearances: ")
(displayln (app input))
