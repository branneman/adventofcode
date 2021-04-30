#lang racket

(require "reindeer-olympics.rkt")

(define input-file
  (let* ([from (find-system-path 'orig-dir)]
         [to (find-system-path 'run-file)]
         [dir (path-only (find-relative-path from to))]
         [base (if (false? dir) 'same dir)])
    (build-path base "input")))

(define lines (file->lines input-file))

(display "distance of winning reindeer: ")
(displayln (winner-distance lines 2503))

(display "points of winning reindeer: ")
(displayln (winner-points lines 2503))
