#lang racket

(require "accounting.rkt"
         json)

(define input-file
  (let* ([from (find-system-path 'orig-dir)]
         [to (find-system-path 'run-file)]
         [dir (path-only (find-relative-path from to))]
         [base (if (false? dir) 'same dir)])
    (build-path base "input.json")))

(define input (read-json (open-input-file input-file)))

(display "sum of all numbers: ")
(total input)
