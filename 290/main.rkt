#lang errortrace racket

(define-syntax-rule (≠ x y) (not (= x y)))
(define-syntax-rule (string≠? x y) (not (string=? x y)))

(define/contract (word-pattern pattern s)
  (-> string? string? boolean?)

  (let/ec return
    (define words (string-split s))

    (when (≠ (string-length pattern) (length words))
      (return #f))

    (for/fold ([map (hash)])
              ([c pattern]
               [word words])

      (if (or
           (and (not (hash-has-key? map c))
                (member word (hash-values map)))
           (and (hash-has-key? map c)
                (string≠? (hash-ref map c) word)))

          (return #f)

          (hash-set map c word)))

    #t))

(word-pattern "aaa" "aa aa aa aa")
