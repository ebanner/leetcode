#lang racket

(require racket/generator)

(define-syntax-rule (>> n) (arithmetic-shift n -1))
(define-syntax-rule (% n) (remainder n 2))

(define (in-bits n)
  (in-generator
   (let loop ([n n])
     (when (positive? n)
       (yield (% n))
       (loop (>> n))))))

(define/contract (hamming-weight n)
  (-> exact-integer? exact-integer?)

  (for/sum ([bit (in-bits n)]
             #:when (= bit 1))
    1))

(hamming-weight 2147483645)
