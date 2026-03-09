#lang racket

(define (ZERO) 0)

(define (get-counts t)
  (for/fold ([counts (hash)])
            ([c t])
    (hash-update counts c add1 ZERO)))

(define (get-remaining-value counts)
  (for/first ([(c count) (in-hash counts)]
              #:when (= count 1))
    c))

(define/contract (find-the-difference s t)
  (-> string? string? char?)

  (for/fold ([counts (get-counts t)]
             #:result (get-remaining-value counts))
            ([c s])

    (hash-update counts c sub1)))

(find-the-difference "" "y")
