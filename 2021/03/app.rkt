#lang racket/base

(require racket/path
         racket/file
         "diagnostic.rkt")

(define input-file
  (let* ([from (find-system-path 'orig-dir)]
         [to (find-system-path 'run-file)]
         [dir (path-only (find-relative-path from to))]
         [base (if (not dir) 'same dir)])
    (build-path base "input")))

(define input (file->lines input-file))

(display "power consumption: ")
(displayln (app input))
