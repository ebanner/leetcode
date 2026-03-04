#lang racket

(define (get-counts s)
  (for/fold ([counts (hash)])
            ([c s])

    (if (hash-has-key? counts c)
        (hash-set counts c (add1 (hash-ref counts c)))
        (hash-set counts c 1))))

(define/contract (is-anagram s t)
  (-> string? string? boolean?)

  (equal? (get-counts s) (get-counts t)))

(is-anagram "rat" "car")
