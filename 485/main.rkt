#lang racket

(define/contract (find-max-consecutive-ones nums)
  (-> (listof exact-integer?) exact-integer?)

  (for/fold ([max-num-ones 0]
             [num-ones 0]
             #:result max-num-ones)
            ([num nums])

    (if (zero? num)
        (values max-num-ones 0)
        (let ([new-num-ones (add1 num-ones)])
          (values (max new-num-ones max-num-ones)
                  new-num-ones)))))

;; (find-max-consecutive-ones '(1 1 0 1 1 1))

;; (find-max-consecutive-ones '[1 0 1 1 0 1])

