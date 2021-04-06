#lang racket

(provide operation
         tokenise
         parse
         16-bit
         operator->fn
         circuit
         solve
         app-part1
         app-part2)

(struct operation (operator operands wire) #:transparent)

(define (tokenise s)
  (map (λ (x)
         (cond
           [(member x '("NOT" "OR" "AND" "LSHIFT" "RSHIFT"))
            (string->symbol (string-downcase x))]
           [(regexp-match? #px"^\\d+$" x)
            (string->number x)]
           [else
            x]))
       (string-split s)))

(define (parse tokens)
  (cond
    [(= 3 (length tokens))
     (operation
      'nop
      (list (first tokens))
      (last tokens))]
    [(= 4 (length tokens))
     (operation
      'not
      (list (second tokens))
      (last tokens))]
    [(= 5 (length tokens))
     (operation
      (second tokens)
      (list (first tokens) (third tokens))
      (last tokens))]))

(define (16-bit x)
  (modulo x 65536))

(define (operator->fn s)
  (compose (λ (x) (16-bit x))
           (cond
             [(equal? s 'nop) identity]
             [(equal? s 'not) bitwise-not]
             [(equal? s 'or) bitwise-ior]
             [(equal? s 'and) bitwise-and]
             [(equal? s 'lshift) arithmetic-shift]
             [(equal? s 'rshift) (λ (x y) (arithmetic-shift x (- y)))])))

(define (circuit xs)
  (foldl (λ (s h)
           (let* ([op (parse (tokenise s))]
                  [k (operation-wire op)])
             (hash-set h k op)))
         (make-immutable-hash)
         xs))

(define (solve board)
  (define (f state queue)
    (if (empty? queue)
        state
        (let* ([op (car queue)]
               [operands (map (λ (x)
                                (if (number? x) x (hash-ref state x #f)))
                              (operation-operands op))])
          (if (andmap number? operands)
              (f (hash-set state (operation-wire op) 
                           (apply
                            (operator->fn (operation-operator op))
                            operands))
                 (rest queue))
              (f state
                 (append (rest queue) (list (first queue))))))))
  (f (make-immutable-hash) (hash-values board)))

(define (app-part1 xs)
  (solve (circuit xs)))

(define (app-part2 xs)
  (solve (hash-set
          (circuit xs)
          "b"
          (operation 'nop '(956) "b"))))
