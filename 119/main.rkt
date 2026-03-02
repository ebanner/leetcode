#lang racket

(define (get-next-row row)
  (append
   '(1)
   (for/list ([a row]
              [b (rest row)])
     (+ a b))
   '(1)))

(define/contract (get-row row-index)
  (-> exact-integer? (listof exact-integer?))

  (for/fold ([row '(1)])
            ([_ (in-range row-index)])

    (get-next-row row)))

(get-row 1)
