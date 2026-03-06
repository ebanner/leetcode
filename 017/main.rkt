#lang errortrace racket

(define NUMPAD
  #hash((#\1 . "")     (#\2 . "abc") (#\3 . "def")
        (#\4 . "ghi")  (#\5 . "jkl") (#\6 . "mno")
        (#\7 . "pqrs") (#\8 . "tuv") (#\9 . "wxyz")))

(define/contract (letter-combinations digits)
  (-> string? (listof string?))

  (match (string->list digits)
    ['() '("")]

    [(cons first-char rest-chars)

     (define letters (hash-ref NUMPAD first-char))

     (append*
      (for/list ([letter letters])
        (for/list ([combination
                    (letter-combinations (list->string rest-chars))])
          (string-append (string letter) combination))))]))

(letter-combinations "23")
