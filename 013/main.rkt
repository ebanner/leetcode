#lang racket

(define-syntax-rule (≠ x y) (not (= x y)))
(define-syntax-rule (in? H k) (hash-has-key? H k))

(define H (hash #\I 1
                #\V 5
                #\X 10
                #\L 50
                #\C 100
                #\D 500
                #\M 1000
                "IV" 4
                "IX" 9
                "XL" 40
                "XC" 90
                "CD" 400
                "CM" 900))

(define (safe-substring s start [end (string-length s)])
  (define len (string-length s))
  (define s* (max 0 (min start len)))
  (define e* (max s* (min end len)))
  (substring s s* e*))

(define/contract (roman-to-int s)
  (-> string? exact-integer?)

  (define N (string-length s))

  (define value

    (let/ec return

      (let loop ([value 0]
                 [i 0])

        (let/ec continue

          (when (= i N)
            (return value))

          (when (in? H (safe-substring s i (+ i 2)))
            (let ([new-value (+ value
                                (hash-ref H (safe-substring s i (+ i 2))))])
              (continue (loop new-value (+ i 2)))))

          (define c (string-ref s i))

          (loop (+ (hash-ref H c) value)
                (add1 i))))))

  value)

