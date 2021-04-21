#lang racket

(require "corporate-policy.rkt")

(define pw1 "cqjxjnds")
(define pw2 (next-password pw1))
(define pw3 (next-password pw2))

(display "first next password: ")
(displayln pw2)

(display "second next password: ")
(displayln pw3)
