#lang racket

(define (get-clippings magazine)
  (for/fold ([counts (hash)])
            ([letter magazine])

    (hash-set counts
              letter
              (add1
               (hash-ref counts letter 0)))))

(define (has-clipping? clippings letter)
  (and
   (hash-has-key? clippings letter)
   (positive?
    (hash-ref clippings letter))))

(define (remove-clipping clippings letter)
  (hash-set clippings
            letter
            (sub1
             (hash-ref clippings letter))))

(define/contract (can-construct ransom-note magazine)
  (-> string? string? boolean?)

  (let/ec return
    (for/fold ([clippings (get-clippings magazine)])
              ([letter ransom-note])

      (when (not (has-clipping? clippings letter))
        (return #f))

      (remove-clipping clippings letter))

    #t))

(can-construct "aa" "ab")
