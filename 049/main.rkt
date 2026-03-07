#lang racket

(define-syntax-rule (list-append lst x)
  (append lst (list x)))

(define (string-sort str)
  (list->string (sort (string->list str) char<?)))

(define (get-anagrams-hash strs)
  (for/fold ([anagrams-hash (hash)])
            ([str strs])

    (define anagram-key (string-sort str))

    (hash-set anagrams-hash
              anagram-key
              (list-append (hash-ref anagrams-hash anagram-key '())
                           str))))

(define/contract (group-anagrams strs)
  (-> (listof string?) (listof (listof string?)))

  (define anagrams-hash (get-anagrams-hash strs))
  (hash-values anagrams-hash))

(group-anagrams '("eat" "tea" "tan" "ate" "nat" "bat"))
