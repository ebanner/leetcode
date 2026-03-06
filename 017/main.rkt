#lang errortrace racket

(define-syntax-rule (empty-string? s) (string=? s ""))

(define NUMPAD
  #hash((#\1 . "")     (#\2 . "abc") (#\3 . "def")
        (#\4 . "ghi")  (#\5 . "jkl") (#\6 . "mno")
        (#\7 . "pqrs") (#\8 . "tuv") (#\9 . "wxyz")))

(define/contract (letter-combinations digits)
  (-> string? (listof string?))

  (let/ec return
    (when (empty-string? digits) (return '("")))

    (define-values (first-letter rest-letters)
      (values (string-ref digits 0)
              (substring digits 1)))

    (define letters (hash-ref NUMPAD first-letter))

    (append*
     (for/list ([letter letters])

       (for/list ([combination
                   (letter-combinations rest-letters)])
         (string-append (string letter) combination))))))

(letter-combinations "23")
