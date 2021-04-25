#lang racket

(provide app)

(module+ test (require rackunit))

(define (distance-per-time distance-per-sec sec-travel sec-resting total-sec)
  (let* ([distance-per-phase (* distance-per-sec sec-travel)]
         [time-per-phase (+ sec-travel sec-resting)]
         [n-phases (quotient total-sec time-per-phase)]
         [rest-sec (modulo total-sec time-per-phase)]
         [distances-all-phases (* n-phases distance-per-phase)]
         [rest-distance (cond
                          [(zero? rest-sec) 0]
                          [(< rest-sec sec-travel) (* sec-travel rest-sec)]
                          [(< rest-sec time-per-phase) distance-per-phase]
                          [else (error "huh?")])])
    (+ distances-all-phases rest-distance)))
(module+ test
  (check-equal?
   (distance-per-time 14 10 127 1000)
   1120)
  (check-equal?
   (distance-per-time 16 11 162 1000)
   1056))

(define parse-re
  #px"(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds.")
(define (parse s)
  (let ([l (regexp-match parse-re s)])
    (list (list-ref l 1)
          (string->number (list-ref l 2))
          (string->number (list-ref l 3))
          (string->number (list-ref l 4)))))
(module+ test
  (check-equal?
   (parse "Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds.")
   (list "Dancer" 27 5 132)))

(define (app xs total-sec)
  (foldl (λ (curr acc)
           (let* ([reindeer (parse curr)]
                  [dist (apply
                         distance-per-time
                         (append (cdr reindeer) (list total-sec)))])
             (cond
               [(> dist acc) dist]
               [else acc])))
         0 xs))
