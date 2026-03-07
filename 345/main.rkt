#lang racket

(require data/gvector)

(define-syntax ~>
  (syntax-rules ()
    [(_ x) x]
    [(_ x f rest ...) (~> (f x) rest ...)]))

(define (gvector-add vec x) (begin0 vec (gvector-add! vec x)))

(define-values (vowels VOWELS)
  (values '(#\a #\e #\i #\o #\u)
          '(#\A #\E #\I #\O #\U)))

(define (is-vowel? c)
  (or
   (member c vowels)
   (member c VOWELS)))

(define (get-vowels s)
  (for/list ([c s]
             #:when (is-vowel? c))
    c))

(define/contract (reverse-vowels s)
  (-> string? string?)

  (define vowels (get-vowels s))
  (define reversed-vowels (reverse vowels))

  (define N (string-length s))

  (for/fold ([reversed-s (make-gvector)]
             [j 0]
             #:result (~> reversed-s gvector->list list->string))
            ([c s])

    (if (is-vowel? c)
        (values (gvector-add reversed-s (list-ref reversed-vowels j))
                (add1 j))
        (values (gvector-add reversed-s c)
                j))))

(reverse-vowels "leetcode")
