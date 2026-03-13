#lang racket

(define (get-bitmap nums)
  (define N (length nums))
  (make-vector (add1 N) 0))

(define/contract (find-disappeared-numbers nums)
  (-> (listof exact-integer?) (listof exact-integer?))

  (define bitmap (get-bitmap nums))

  (for ([num nums])
    (vector-set! bitmap num 1))

  (rest
   (for/list ([(bit i) (in-indexed bitmap)]
              #:when (zero? bit))
     i)))

(find-disappeared-numbers '(1 1))
