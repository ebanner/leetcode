#lang racket

(define/contract (convert-to-title column-number)
  (-> exact-integer? string?)

  (define result
   (let loop ([column-number column-number])
     (if (zero? column-number)
         '()
         (append (loop (quotient column-number 26))
               (list (remainder column-number 26))))))

  (displayln result)

  (list->string
   (for/list ([d result])
     (integer->char (+ d 64)))))

(convert-to-title 701)
