#lang racket

(require racket/generator)

(define-syntax-rule (% n) (remainder n 10))
(define-syntax-rule (>> n) (quotient n 10))

(define (in-digits n)
  (in-generator
   (let loop ([n n])
     (when (positive? n)
       (yield (% n))
       (loop (>> n))))))

(define/contract (is-happy n)
  (-> exact-integer? boolean?)

  (let loop ([seen (set)]
             [n n])

    (cond [(set-member? seen n) #f]
          [(= n 1) #t]
          [else (loop (set-add seen n)
                      (for/sum ([d (in-digits n)]) (expt d 2)))])))

(is-happy 2)
