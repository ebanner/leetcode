#lang racket

(define-syntax ~>
  (syntax-rules ()
    [(_ x) x]
    [(_ x f rest ...) (~> (f x) rest ...)]))

(define (divides? k n)
  (zero? (remainder n k)))

(define/contract (construct-rectangle area)
  (-> exact-integer? (listof exact-integer?))

  (sort
   (let loop ([factor (~> area sqrt round inexact->exact)])
     (if (divides? factor area)
         (list factor (quotient area factor))
         (loop (add1 factor))))
   >))

;; (construct-rectangle 4)

;; (construct-rectangle 37)

;; (construct-rectangle 122122)

