#lang racket

(require racket/generator)

(define (in-digits n)
  (in-generator
   (let loop ([n n])
     (when (positive? n)
       (yield (remainder n 10))
       (loop (quotient n 10))))))

(define/contract (add-digits num)
  (-> exact-integer? exact-integer?)

  (let loop ([num num])
    (if (< num 10)
        num
        (loop (for/sum ([d (in-digits num)]) d)))))

(add-digits 0)
