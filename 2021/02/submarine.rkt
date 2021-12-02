#lang racket/base

(provide
 app)
(require
 racket/string
 racket/match)
(module+ test
  (require rackunit))

(define (parse xs)
  (map (λ (s)
         (match-let ([(list direction unit) (string-split s)])
           (list direction (string->number unit))))
       xs))
(module+ test
  (test-case "parse"
    (check-equal?
     (parse '("forward 5" "down 5"))
     '(("forward" 5)
       ("down" 5)))))

(define (run pos depth aim xs)
  (foldl (λ (curr acc)
           (match-let ([(list p d a) acc]
                       [(list direction unit) curr])
             (cond [(string=? direction "forward")
                    (list (+ p unit)
                          (+ d (* a unit))
                          a)]
                   [(string=? direction "down")
                    (list p
                          d
                          (+ a unit))]
                   [(string=? direction "up")
                    (list p
                          d
                          (- a unit))])))
         (list pos depth aim) ; init -> acc
         xs))
(module+ test
  (test-case "run"
    (check-equal?
     (run 0 0 0 '(("forward" 5)))
     '(5 0 0))
    (check-equal?
     (run 5 0 0 '(("down" 5)))
     '(5 0 5))
    (check-equal?
     (run 5 0 5 '(("forward" 8)))
     '(13 40 5))))

(define app
  (compose1
   (λ (xs) (* (car xs) (cadr xs)))
   (λ (xs) (run 0 0 0 xs))
   parse))
(module+ test
  (test-case "app"
    (check-equal?
     (app '("forward 5"
            "down 5"
            "forward 8"
            "up 3"
            "down 8"
            "forward 2"))
     900)))
