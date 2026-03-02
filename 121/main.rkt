#lang racket

(define/contract (max-profit prices-list)
  (-> (listof exact-integer?) exact-integer?)

  (define prices (list->vector prices-list))
  (define N (vector-length prices))

  (define best

    (let loop ([left 0]
               [right 1]
               [best 0])

      (let/ec return

        (when (= right N)
          (return best))

        (define profit (max (- (vector-ref prices right)
                               (vector-ref prices left))
                            0))

        (loop (if (< (vector-ref prices right) (vector-ref prices left))
                  right
                  left)
              (add1 right)
              (max profit best)))))

  (inexact->exact best))

(max-profit '(7 6 4 3 1))
