#lang racket/base

(require racket/path
         racket/file
         "sonar-sweep.rkt")

(define input-file
  (let* ([from (find-system-path 'orig-dir)]
         [to (find-system-path 'run-file)]
         [dir (path-only (find-relative-path from to))]
         [base (if (not dir) 'same dir)])
    (build-path base "input")))

(define input-grid (file->lines input-file))

(display "number of increases in a three-measurement sliding window: ")
(displayln (app input-grid))
