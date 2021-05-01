#lang racket

(require "cookie-recipe.rkt")

(define input-file
  (let* ([from (find-system-path 'orig-dir)]
         [to (find-system-path 'run-file)]
         [dir (path-only (find-relative-path from to))]
         [base (if (false? dir) 'same dir)])
    (build-path base "input")))

(define lines (file->lines input-file))

(display "total score of the highest-scoring cookie: ")
(displayln (winning-cookie-score lines))

(display "total score of the highest-scoring cookie with 500 calories: ")
(displayln (winning-cookie-500-calories-score lines))
