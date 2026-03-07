#lang racket

(define/contract (permute nums)
  (-> (listof exact-integer?) (listof (listof exact-integer?)))

  (let get-permutations ([remaining (list->set nums)])
    (let/ec return
      (when (set-empty? remaining)
        (return '(())))

      (append*
       (for/list ([element remaining])
         (for/list ([permutation
                     (get-permutations (set-remove remaining element))])
           (cons element permutation)))))))

(permute '(1 2 3))
