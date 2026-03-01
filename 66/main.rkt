#lang racket

(define/contract (plus-one digits)
  (-> (listof exact-integer?) (listof exact-integer?))

  (let loop ([digits digits]
             [carry 1])

    (let/ec return

      (when (empty? digits)
        (return (if (= carry 1)
                    (list 1)
                    '())))

      (define new-digit (+ carry
                           (last digits)))

      (append (loop (drop-right digits 1) (quotient new-digit 10))
              (list (remainder new-digit 10))))))

(plus-one '(1 2 3))

(plus-one '(4 3 2 1))

(plus-one '(9))


