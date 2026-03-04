#lang racket

(define-syntax-rule (non-zero? x) (not (zero? x)))

(define-syntax ~>
  (syntax-rules ()
    [(_ x) x]
    [(_ x f rest ...) (~> (f x) rest ...)]))

(define (divide-out-of n k)
  (let loop ([n n])
    (if (non-zero? (remainder n k))
        n
        (loop (quotient n k)))))

(define (divide-out k)
  (lambda (n) (divide-out-of n k)))

(define/contract (is-ugly n)
  (-> exact-integer? boolean?)

  (and
   (non-zero? n)
   (=
    (~> n
        (divide-out 2)
        (divide-out 3)
        (divide-out 5))
    1)))

(is-ugly 14)
