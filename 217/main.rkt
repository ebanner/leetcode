#lang racket

(define/contract (contains-duplicate nums)
  (-> (listof exact-integer?) boolean?)

  (let/ec return
   (for/fold ([seen (set)])
             ([num nums])

     (when (set-member? seen num)
       (return #t))

     (set-add seen num))

    #f))

(contains-duplicate '(1 2 3 4))
