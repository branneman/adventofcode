#lang racket

(provide winner-distance
         winner-points)

(module+ test (require rackunit))

(struct speed (distance-per-sec sec-travel sec-resting) #:transparent)
(struct status (sec-travel sec-resting) #:transparent)
(struct reindeer (name speed status distance points) #:transparent)

(define (distance-per-time r total-sec)
  (let* ([distance-per-sec (speed-distance-per-sec (reindeer-speed r))]
         [sec-travel (speed-sec-travel (reindeer-speed r))]
         [sec-resting (speed-sec-resting (reindeer-speed r))]

         [distance-per-phase (* distance-per-sec sec-travel)]
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
  (test-case "distance-per-time"
    (check-equal?
     (distance-per-time (reindeer "Comet" (speed 14 10 127) (status 10 127) 0 0) 1000)
     1120)
    (check-equal?
     (distance-per-time (reindeer "Dancer" (speed 16 11 162) (status 11 162) 0 0) 1000)
     1056)))

(define (move-one-second r)
  (let ([sec-travel (status-sec-travel (reindeer-status r))]
        [sec-resting (status-sec-resting (reindeer-status r))])
    (cond
      [(and (zero? sec-travel) (zero? sec-resting))
       (reindeer (reindeer-name r)
                 (reindeer-speed r)
                 (status (sub1 (speed-sec-travel (reindeer-speed r)))
                         (speed-sec-resting (reindeer-speed r)))
                 (+ (speed-distance-per-sec (reindeer-speed r))
                    (reindeer-distance r))
                 (reindeer-points r))]

      [(zero? sec-travel)
       (reindeer (reindeer-name r)
                 (reindeer-speed r)
                 (status sec-travel (sub1 sec-resting))
                 (reindeer-distance r)
                 (reindeer-points r))]

      [else
       (reindeer (reindeer-name r)
                 (reindeer-speed r)
                 (status (sub1 sec-travel) sec-resting)
                 (+ (speed-distance-per-sec (reindeer-speed r))
                    (reindeer-distance r))
                 (reindeer-points r))])))
(module+ test
  (test-case "move-one-second"
    (check-equal?
     (move-one-second (reindeer "Comet" (speed 14 10 127) (status 10 127) 0 0))
     (reindeer "Comet" (speed 14 10 127) (status 9 127) 14 0))
    (check-equal?
     (move-one-second (reindeer "Comet" (speed 14 10 127) (status 0 120) 13 37))
     (reindeer "Comet" (speed 14 10 127) (status 0 119) 13 37))
    (check-equal?
     (move-one-second (reindeer "Dancer" (speed 16 11 162) (status 0 0) 0 0))
     (reindeer "Dancer" (speed 16 11 162) (status 10 162) 16 0))))

(define (award-points xs)
  (define highest-distance
    (for/fold ([sum empty])
              ([r xs])
      (cond
        [(empty? sum)
         (cons r sum)]
        [(> (reindeer-distance r) (reindeer-distance (car sum)))
         (cons r empty)]
        [(= (reindeer-distance r) (reindeer-distance (car sum)))
         (cons r sum)]
        [else sum])))
  (for/list ([r xs])
    (cond
      [(member r highest-distance)
       (reindeer (reindeer-name r)
                 (reindeer-speed r)
                 (reindeer-status r)
                 (reindeer-distance r)
                 (add1 (reindeer-points r)))]
      [else r])))
(module+ test
  (test-case "award-points"
    (check-equal?
     (award-points empty)
     empty)
    (check-equal?
     (award-points (list (reindeer "Comet" null null 150 0)
                         (reindeer "Dancer" null null 100 0)))
     (list (reindeer "Comet" null null 150 1)
           (reindeer "Dancer" null null 100 0)))
    (check-equal?
     (award-points (list (reindeer "Comet" null null 150 13)
                         (reindeer "Dancer" null null 170 36)))
     (list (reindeer "Comet" null null 150 13)
           (reindeer "Dancer" null null 170 37)))
    (check-equal?
     (award-points (list (reindeer "Comet" null null 42 3)
                         (reindeer "Rudolph" null null 40 3)
                         (reindeer "Dancer" null null 42 3)))
     (list (reindeer "Comet" null null 42 4)
           (reindeer "Rudolph" null null 40 3)
           (reindeer "Dancer" null null 42 4)))))

(define (race-with-points xs total-sec)
  (define (f l n)
    (cond
      [(zero? n) l]
      [else (f (award-points (map move-one-second l))
               (sub1 n))]))
  (f xs total-sec))
(module+ test
  (test-case "race-with-points"
    (let* ([actual
            (race-with-points (list (reindeer "Comet" (speed 14 10 127) (status 10 127) 0 0)
                                    (reindeer "Dancer" (speed 16 11 162) (status 11 162) 0 0))
                              1000)]
           [expected (list (reindeer "Comet" (speed 14 10 127) (reindeer-status (car actual)) 1120 312)
                           (reindeer "Dancer" (speed 16 11 162) (reindeer-status (cadr actual)) 1056 689))])
      (check-equal? actual expected))))

(define parse-re
  #px"(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds.")
(define (parse s)
  (let ([l (regexp-match parse-re s)])
    (reindeer (list-ref l 1)
              (speed (string->number (list-ref l 2))
                     (string->number (list-ref l 3))
                     (string->number (list-ref l 4)))
              (status (string->number (list-ref l 3))
                      (string->number (list-ref l 4)))
              0
              0)))
(module+ test
  (test-case "parse"
    (check-equal?
     (parse "Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds.")
     (reindeer "Dancer" (speed 27 5 132) (status 5 132) 0 0))))

(define (winner-distance xs total-sec)
  (apply max (map (λ (x) (distance-per-time (parse x) total-sec))
                  xs)))

(define (winner-points xs total-sec)
  (apply max (map reindeer-points
                  (race-with-points (map parse xs) total-sec))))
