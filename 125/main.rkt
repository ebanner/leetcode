#lang racket

(define-syntax ~>
  (syntax-rules ()
    [(_ x) x]
    [(_ x f rest ...) (~> (f x) rest ...)]))

(define (string-reverse s)
  (list->string
   (reverse (string->list s))))

(define (is-alphanumeric? c)
  (or (char-alphabetic? c)
      (char-numeric? c)))

(define (remove-non-alphanumeric s)
  (list->string
   (for/list ([c s]
              #:when (is-alphanumeric? c))
     c)))

(define/contract (is-palindrome s)
  (-> string? boolean?)

  (define result
    (~> s
        string-downcase
        remove-non-alphanumeric))

  (string=? (string-reverse result) result))

(is-palindrome "A man, a plan, a canal: Panama")
