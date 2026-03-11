#lang racket

(define (get-bitmap)
  (for/hash ([c
              "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"])
    (values c 0)))

(define (contains-1? bitmap)
  (member 1 (hash-values bitmap)))

(define (XOR1 b) (bitwise-xor b 1))

(define/contract (longest-palindrome s)
  (-> string? exact-integer?)

  (for/fold ([bitmap (get-bitmap)]
             [palindrome-length 0]
             #:result (+ (if (contains-1? bitmap)
                             (add1 palindrome-length)
                             palindrome-length)))
            ([c s])

    (values (hash-update bitmap c XOR1)
            (+ (if (positive? (hash-ref bitmap c)) 2 0)
               palindrome-length))))

(longest-palindrome "bb")
