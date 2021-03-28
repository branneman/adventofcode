#lang racket

(require openssl/md5)

(provide starts-with
         app)

(define (starts-with s str)
  (if (>= (string-length str) (string-length s))
      (equal? s (substring str 0 (string-length s)))
      #f))

(define (app padding)
  (define (f n)
    (let* ([s (string-append padding (~s n))]
           [h (md5 (open-input-string s))])
      (if (starts-with "000000" h)
          n
          (f (+ 1 n)))))
  (f 0))
