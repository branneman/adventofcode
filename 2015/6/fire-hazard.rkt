#lang racket

(provide action
         make-grid
         grid-cell
         grid-cell!
         flip-switch
         execute
         action->symbol
         parse
         count-lit
         app)

(struct action (turn x1 y1 x2 y2) #:transparent)

(define (make-grid size v)
  (list->vector (for/list ([i size]) ; meh
                  (make-vector size v))))

(define (grid-cell grid x y)
  (vector-ref (vector-ref grid x) y))

(define (grid-cell! grid x y v)
  (vector-set! (vector-ref grid x) y v)
  grid)

(define (flip-switch v turn)
  (cond
    [(equal? turn 'turn-on)
     #t]
    [(equal? turn 'turn-off)
     #f]
    [(equal? turn 'toggle)
     (if (false? v) #t #f)]))

(define (execute grid a)
  (let ([x1 (action-x1 a)]
        [y1 (action-y1 a)]
        [x2 (action-x2 a)]
        [y2 (action-y2 a)])
    (for/list ([x (range x1 (+ 1 x2))])
      (for/list ([y (range y1 (+ 1 y2))])
        (grid-cell! grid x y (flip-switch (grid-cell grid x y)
                                          (action-turn a)))))
    grid))

(define (action->symbol s)
  (case s
    [("turn on") 'turn-on]
    [("turn off") 'turn-off]
    [("toggle") 'toggle]))

(define (parse s)
  (let ([turn (regexp-match* #px"turn on|turn off|toggle" s)]
        [digits (regexp-match* #px"\\d{1,3}" s)])
    (apply action (append (map action->symbol turn)
                          (map string->number digits)))))

(define (count-lit grid)
  (foldl (λ (curr acc)
           (+ acc (foldl (λ (inner-curr inner-acc)
                           (+ inner-acc
                              (if (false? inner-curr) 0 1)))
                         0
                         (vector->list curr))))
         0
         (vector->list grid)))

(define (app xs)
  (define grid (make-grid 1000 #f))
  (for/list ([s xs])
    (execute grid (parse s)))
  (count-lit grid))
