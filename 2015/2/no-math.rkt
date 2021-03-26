#lang racket

(provide present->box
         surface-box+slack
         total-surface)

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

(define (total-surface boxes)
  (apply + (map (compose1 surface-box+slack present->box)
                boxes)))
