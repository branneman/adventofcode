#lang racket

(require "eggnog-containers.rkt")

(define input-file
  (let* ([from (find-system-path 'orig-dir)]
         [to (find-system-path 'run-file)]
         [dir (path-only (find-relative-path from to))]
         [base (if (false? dir) 'same dir)])
    (build-path base "input")))

(define containers (map string->number
                        (file->lines input-file)))

(display "different combinations of containers: ")
(displayln (length (container-combinations 150 containers)))

(display "number of different ways with minimum number of containers: ")
(displayln (length (minimum-containers-combinations 150 containers)))
