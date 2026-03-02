#lang racket

(define (get-next-row row)
  (append
   '(1)
   (for/list ([a row]
              [b (rest row)])
     (+ a b))
   '(1)))

(define/contract (generate num-rows)
  (-> exact-integer? (listof (listof exact-integer?)))

  (let loop ([row '(1)]
             (n num-rows))

    (if (zero? n)
        '()
        (cons row (loop (get-next-row row) (sub1 n))))))

(generate 1)
