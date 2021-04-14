#lang racket

(provide number->char
         look-and-say
         call-n
         app)

(define (number->char n)
  (car (string->list (~s n))))

(define (look-and-say s)
  (define (f l acc c n)
    (cond
      [(or (empty? l) (empty? (cdr l)))
       (cons c (cons (number->char n) acc))]

      [(equal? c (cadr l))
       (f (cdr l)
          acc
          c
          (add1 n))]

      [else
       (f (cdr l)
          (cons c (cons (number->char n) acc))
          (cadr l)
          1)]))

  (let* ([l (string->list s)]
         [x (f l empty (car l) 1)]
         [y (list->string (reverse x))])
    y))

(define (call-n n acc f)
  (cond
    [(zero? n) acc]
    [else (call-n (sub1 n) (f acc) f)]))

(define (app s n)
  (call-n n s look-and-say))
