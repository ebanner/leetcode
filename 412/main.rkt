#lang racket

(define/contract (fizz-buzz N)
  (-> exact-integer? (listof string?))

  (for/list ([n (in-range 1 (add1 N))])
    (cond [(zero? (remainder n 15)) "FizzBuzz"]
          [(zero? (remainder n 3)) "Fizz"]
          [(zero? (remainder n 5)) "Buzz"]
          [else (number->string n)])))

(fizz-buzz 15)
