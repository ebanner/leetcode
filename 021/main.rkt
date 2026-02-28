#lang errortrace racket

(define-syntax-rule (≤ x y) (<= x y))

; Definition for singly-linked list:
#|

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
(list-node val #f))

|#

(define/contract (merge-two-lists list1 list2)
  (-> (or/c list-node? #f) (or/c list-node? #f)
      (or/c list-node? #f))

  (let/ec return

    (when (and (not list1) (not list2))
      (return #f))

    (when (not list1)
      (return (list-node (list-node-val list2)
                         (merge-two-lists list1
                                          (list-node-next list2)))))

    (when (not list2)
      (return (list-node (list-node-val list1)
                         (merge-two-lists (list-node-next list1)
                                          list2))))

    (if (≤ (list-node-val list1) (list-node-val list2))
        (list-node (list-node-val list1)
                   (merge-two-lists (list-node-next list1)
                                    list2))
        (list-node (list-node-val list2)
                   (merge-two-lists list1 (list-node-next list2))))))

(define merged
  (let ([list1 (make-list-node 5)]
        [list2 (make-list-node 4)])

    (merge-two-lists list1 list2)))

(let ([list1 (list-node 1 (list-node 2 (make-list-node 4)))]
      [list2 (list-node 1 (list-node 3 (make-list-node 4)))])

  (merge-two-lists list1 list2))
