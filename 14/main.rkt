#lang errortrace racket

(require racket/list)

(define (all-equal? chars)
  (for/and ([c chars])
    (char=? c (first chars))))

(define/contract (longest-common-prefix strs)
  (-> (listof string?) string?)

  (define N (apply min
                   (for/list ([str strs]) (string-length str))))

  (define failing-index
    (for/first ([i (in-naturals)]
                #:when (or
                        (= i N)
                        (not (all-equal? (for/list ([str strs]) (string-ref str i))))))
      i))

  (substring (first strs) 0 failing-index))
