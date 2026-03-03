#lang racket

(define/contract (length-of-longest-substring s)
  (-> string? exact-integer?)

  (define N (string-length s))

  (let loop ([left 0]
             [right 0]
             [window (set)]
             [biggest 0])

    (let/ec return
      (when (= right N) (return biggest))

      (if (set-member? window (string-ref s right))
          (loop (add1 left)
                right
                (set-remove window (string-ref s left))
                biggest)

          (loop left
                (add1 right)
                (set-add window (string-ref s right))
                (max (add1 (- right left)) biggest))))))


(length-of-longest-substring "pwwkew")
