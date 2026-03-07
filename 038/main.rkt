#lang errortrace racket

(define-syntax-rule (list-append lst x) (append lst (list x)))

(define (encode s j i)
  (string-append (number->string (- i j))
                 (string
                  (string-ref s j))))

(define (get-rle s)
  (define N (string-length s))

  (string-join
   (for/fold ([rle '()]
              [j 0]
              #:result (list-append rle (encode s j N)))
             ([i (in-range 1 N)])

     (if (char=? (string-ref s j) (string-ref s i))
         (values rle j)
         (values (list-append rle (encode s j i))
                 i)))
   ""))

(define/contract (count-and-say n)
  (-> exact-integer? string?)

  (if (= n 1)
      "1"
      (get-rle
       (count-and-say (sub1 n)))))

(count-and-say 1)
