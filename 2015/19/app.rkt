#lang racket/base

(require racket/path
         racket/file
         "medicine.rkt")

(define (input-file filename)
  (let* ([from (find-system-path 'orig-dir)]
         [to (find-system-path 'run-file)]
         [dir (path-only (find-relative-path from to))]
         [base (if (not dir) 'same dir)])
    (build-path base filename)))

(define input-replacements (file->lines (input-file "input-replacements")))
(define input-molecule (file->string (input-file "input-molecule")))

(display "number of distinct molecules: ")
(displayln (app input-replacements input-molecule))
