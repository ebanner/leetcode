#lang racket

(define (tiles? t s)
  (define-values (M N)
    (values (string-length t)
            (string-length s)))

  (let/ec return
    (when (positive? (remainder N M))
      (return #f))

    (let loop ([s s])
      (cond [(string=? s "") #t]
            [(not (string=? t (substring s 0 M))) #f]
            [else (loop (substring s M))]))))

(define/contract (repeated-substring-pattern s)
  (-> string? boolean?)

  (define N (string-length s))

  (for/first ([i (in-range 1 N)]
              #:when (tiles? (substring s 0 i) s))
    #t))

;; (repeated-substring-pattern "abab")

;; (repeated-substring-pattern "aba")

;; (repeated-substring-pattern "abcabcabcabc")

;; (repeated-substring-pattern "bb")

