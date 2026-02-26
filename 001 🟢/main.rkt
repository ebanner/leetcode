#lang racket

(define/contract (two-sum nums-list target)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))

  (define nums (list->vector nums-list))

  (define N (vector-length nums))

  (for*/first ([i (in-range (sub1 N))]
               [j (in-range (add1 i) N)]
               #:when (= (+ (vector-ref nums i) (vector-ref nums j))
                         target))
    (list i j)))
