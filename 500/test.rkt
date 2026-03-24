#lang racket

(require "main.rkt")

(require rackunit)

(define (test result expected)
  (check-equal? result expected)

  (if (equal? result expected)
    (display "🟢")
    (display "❌")))

(module+ main
  (test
   (find-words '("Alaska"))
   '("Alaska"))

  (test
   (find-words '("Hello" "Alaska" "Dad" "Peace"))
   '("Alaska" "Dad"))

  (test
   (find-words '("omk"))
   '())

  (test
   (find-words '("adsdf" "sfd"))
   '("adsdf" "sfd"))

  (test
   (find-words '("a" "b"))
   '("a" "b"))

  (test
   (find-words '("qwee"))
   '("qwee"))

  (display "\n")

  )
