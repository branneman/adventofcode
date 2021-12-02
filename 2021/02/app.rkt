#lang racket/base

(require racket/path
         racket/file
         "submarine.rkt")

(define input-file
  (let* ([from (find-system-path 'orig-dir)]
         [to (find-system-path 'run-file)]
         [dir (path-only (find-relative-path from to))]
         [base (if (not dir) 'same dir)])
    (build-path base "input")))

(define input (file->lines input-file))

(display "where its going: ")
(displayln (app input))
