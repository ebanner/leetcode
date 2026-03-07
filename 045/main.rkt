#lang racket

(define/contract (jump nums-list)
  (-> (listof exact-integer?) exact-integer?)

  (define nums (list->vector nums-list))
  (define N (vector-length nums))
  (define dp (build-vector N (λ (i) (if (zero? i) 0 10000))))

  (for ([i N])
    (for ([j (add1 (vector-ref nums i))])
      (define safe-j (min (+ i j)
                          (sub1 N)))
      (vector-set! dp
                   safe-j
                   (min (+ (vector-ref dp i) 1)
                        (vector-ref dp safe-j)))))

  (vector-ref dp (sub1 N)))

(jump '(2 3 0 1 4))
