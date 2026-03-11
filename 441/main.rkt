#lang racket

(define ∞ (in-naturals 1))

(define/contract (arrange-coins n)
  (-> exact-integer? exact-integer?)

  (let/ec return
   (for/fold ([n n])
             ([i ∞])

     (when (< n i)
       (return (sub1 i)))

     (- n i))))

(arrange-coins 5)
