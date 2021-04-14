#lang racket

(require "look-and-say.rkt")

(define input "1321131112")

(define look-and-say-40 (app input 40))
(display "result length after 40x look-and-say: ")
(string-length look-and-say-40)

(define look-and-say-50 (app look-and-say-40 10))
(display "result length after 50x look-and-say: ")
(string-length look-and-say-50)
