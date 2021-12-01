#lang racket/base

(provide
 app)
(require
 racket/list)
(module+ test
  (require rackunit))

(define (parse xs)
  (map string->number xs))

(define (sliding-window-3 xs)
  (for/list ([i (- (length xs) 2)])
    (list (list-ref xs i)
          (list-ref xs (+ i 1))
          (list-ref xs (+ i 2)))))
(module+ test
  (test-case "sliding-window-3"
    (check-equal?
     (sliding-window-3 '(199 200 208 210 200 207 240 269 260 263))
     '((199 200 208)
       (200 208 210)
       (208 210 200)
       (210 200 207)
       (200 207 240)
       (207 240 269)
       (240 269 260)
       (269 260 263)))))

(define (sum-3d xxs)
  (map (λ (xs) (apply + xs)) xxs))
(module+ test
  (test-case "sum-3d"
    (check-equal?
     (sum-3d '((199 200 208)
               (200 208 210)
               (208 210 200)
               (210 200 207)
               (200 207 240)
               (207 240 269)
               (240 269 260)
               (269 260 263)))
     '(607 618 618 617 647 716 769 792))))

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
(module+ test
  (test-case "count-increases"
    (check-equal?
     (count-increases '(199 200 208 210 200 207 240 269 260 263))
     7)))

(define app
  (compose1 count-increases
            sum-3d
            sliding-window-3
            parse))
