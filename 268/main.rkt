#lang racket

(define/contract (missing-number nums)
  (-> (listof exact-integer?) exact-integer?)

  (define N (length nums))
  (define bitmask (make-vector (add1 N) 0))

  (for ([num nums])
    (vector-set! bitmask num 1))

  (for/first ([(bit i) (in-indexed bitmask)]
              #:when (zero? bit))
    i))

(missing-number '(9 6 4 2 3 5 7 0 1))
