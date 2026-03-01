#lang racket

(define/contract (length-of-last-word s)
  (-> string? exact-integer?)

  (define words (string-split s))

  (define last-word (last words))

  (string-length last-word))

(length-of-last-word "Hello World")

(length-of-last-word "   fly me   to   the moon  ")

(length-of-last-word "luffy is still joyboy")
