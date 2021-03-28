#lang racket

(provide unique
         move
         delivery-run
         unzip
         count-deliveries
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

(define (unzip xs)
  (define (f a b xs)
    (cond
      [(= 1 (length xs))
       (list (append a (list (car xs)))
             b)]
      [(= 0 (length xs))
       (list a b)]
      [else
       (f (append a (list (car xs)))
          (append b (list (cadr xs)))
          (drop xs 2))]))
  (f '() '() xs))

(define count-deliveries
  (compose length
           unique
           append))

(define (app str)
  (let* ([xs (string->list str)]
         [split (unzip xs)]
         [santa (car split)]
         [robot (cadr split)])
    (count-deliveries (delivery-run santa)
                      (delivery-run robot))))
