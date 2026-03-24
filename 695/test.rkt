#lang racket

(require "main.rkt")

(require rackunit)

(define (test result expected)
  (check-equal? result expected)

  (if (equal? result expected)
      (display "🟢")
      (display "❌")))

(module+ main
  (let ([island '((0 0 1 0 0 0 0 1 0 0 0 0 0)
                  (0 0 0 0 0 0 0 1 1 1 0 0 0)
                  (0 1 1 0 1 0 0 0 0 0 0 0 0)
                  (0 1 0 0 1 1 0 0 1 0 1 0 0)
                  (0 1 0 0 1 1 0 0 1 1 1 0 0)
                  (0 0 0 0 0 0 0 0 0 0 1 0 0)
                  (0 0 0 0 0 0 0 1 1 1 0 0 0)
                  (0 0 0 0 0 0 0 1 1 0 0 0 0))])

    (test (max-area-of-island island)
          6))

  (let ([island '((0 0 0 0 0 0 0 0))])

    (test (max-area-of-island island)
          0))

  (display "\n"))
