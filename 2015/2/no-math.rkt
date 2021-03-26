#lang racket

(provide present->box
         surface-box+slack
         take-2-smallest
         ribbon-length
         total-surface
         total-ribbon)

(define (present->box str)
  (map string->number (string-split str "x")))

(define (surface-box+slack box)
  (let* ([l (car box)]
         [w (cadr box)]
         [h (caddr box)]

         ; surface = 2*l*w + 2*w*h + 2*h*l
         [surface (+ (* 2 l w)
                     (* 2 w h)
                     (* 2 h l))]

         ; slack = area of the smallest side
         [slack (min (* l w)
                     (* w h)
                     (* h l))])
    (+ surface slack)))

(define (take-2-smallest box)
  (take (sort box <) 2))

(define (ribbon-length box)
  (let (; shortest distance around its sides
        [distance (apply + (map (λ (x) (* 2 x))
                                (take-2-smallest box)))]

        ; cubic volume of box
        [volume (apply * box)])
    (+ distance volume)))

(define (total-surface boxes)
  (apply + (map (compose1 surface-box+slack present->box)
                boxes)))

(define (total-ribbon boxes)
  (apply + (map (compose1 ribbon-length present->box)
                boxes)))
