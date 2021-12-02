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

(define (run pos depth xs)
  (foldl (λ (curr acc)
           (let ([p (car acc)]
                 [d (cadr acc)]
                 [direction (car curr)]
                 [unit (cadr curr)])
             (cond [(string=? direction "forward")
                    (list (+ p unit) d)]
                   [(string=? direction "down")
                    (list p (+ d unit))]
                   [(string=? direction "up")
                    (list p (- d unit))])))
         (list pos depth) ; init -> acc
         xs))
(module+ test
  (test-case "run"
    (check-equal?
     (run 0 0 '(("forward" 5)))
     '(5 0))))

(define app
  (compose1
   (λ (xs) (apply * xs))
   (λ (xs) (run 0 0 xs))
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
     150)))
