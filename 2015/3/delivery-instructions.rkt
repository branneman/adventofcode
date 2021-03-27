#lang racket

(provide unique
         move
         delivery-run
         app)

(define (unique xs)
  (foldr (λ (x acc)
           (if (findf (λ (y) (equal? x y)) acc)
               acc
               (cons x acc)))
         '()
         xs))

(define (move loc dir)
  (let ([x (car loc)]
        [y (cadr loc)])
    (case dir
      [(#\^) (list x (- y 1))]
      [(#\>) (list (+ x 1) y)]
      [(#\v) (list x (+ y 1))]
      [(#\<) (list (- x 1) y)])))

(define (delivery-run instructions)
  (foldl (λ (dir acc)
           (cons (move (car acc) dir)
                 acc))
         '((0 0))
         instructions))

(define app
  (compose1 length
            unique
            delivery-run
            string->list))
