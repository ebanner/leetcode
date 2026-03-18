#lang racket

(require racket/generator)

(define-syntax-rule (>> n) (arithmetic-shift n -1))
(define-syntax-rule (% n) (remainder n 2))
(define-syntax-rule (≠ x y) (not (= x y)))

(define (in-bits n)
  (in-generator
   (for/fold ([n n])
             ([_ 32])
     (yield (% n))
     (>> n))))

(define/contract (hamming-distance x y)
  (-> exact-integer? exact-integer? exact-integer?)

  (for/fold ([distance 0])
            ([bx (in-bits x)]
             [by (in-bits y)])
    (+ (if (≠ bx by) 1 0)
       distance)))

;; (hamming-distance 3 1)
