#lang racket

(define-syntax-rule (sub2 n) (sub1 (sub1 n)))

(define MEMO (make-hash))

(define/contract (climb-stairs n)
  (-> exact-integer? exact-integer?)

  (let/ec return

    (cond [(negative? n) (return 0)]
          [((zero? n) (return 1))])

    (when (not (hash-has-key? MEMO n))
      (define num-ways (+ (climb-stairs (sub1 n))
                          (climb-stairs (sub2 n))))
      (hash-set! MEMO n num-ways))

    (hash-ref MEMO n)))

(climb-stairs 35)
