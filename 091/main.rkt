#lang racket

(require racket/string)

(define MEMO (make-hash))

(define/contract (num-decodings s)
  (-> string? exact-integer?)

  (let/ec return
    (when (string=? s "") (return 1))

    (when (hash-has-key? MEMO s) (return (hash-ref MEMO s)))

    (define result
      (for/sum ([i (in-range 1 (add1 26))])
        (let ([i* (number->string i)])
          (if (string-prefix? s i*)
              (num-decodings
               (substring s (string-length i*)))
              0))))

    (hash-set! MEMO s result)

    result))

;; (num-decodings "12")

;; (num-decodings "226")

;; (num-decodings "06")
