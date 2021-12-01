#lang racket/base

(provide
 count-increases
 parse)
(require
 racket/list)

(define (count-increases xs)
  (foldl (λ (curr prev acc)
           (if (> curr prev)
               (add1 acc)
               acc))
         0

         ; xs->curr: original list with the last item appended
         (append xs (list (last xs)))

         ; xs->prev: original list with the first item prepended
         (cons (car xs) xs)))

(define (parse xs)
  (map string->number xs))
