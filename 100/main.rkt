#lang racket

(define-syntax-rule (≠ x y) (not (= x y)))

; Definition for a binary tree node.
#|

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
(tree-node val #f #f))

|#

(define/contract (is-same-tree p q)
  (-> (or/c tree-node? #f) (or/c tree-node? #f) boolean?)

  (let/ec return
    (cond [(and (not p) (not q)) (return #t)]
          [(and (not p) q) (return #f)]
          [(and p (not q)) (return #f)])

    (when (≠ (tree-node-val p) (tree-node-val q))
      (return #f))

    (and (is-same-tree (tree-node-left p)
                       (tree-node-left q))
         (is-same-tree (tree-node-right p)
                       (tree-node-right q)))))

(let ([p (tree-node 1
                    (make-tree-node 2) (make-tree-node 3))]
      [q (tree-node 1
                    (make-tree-node 2) (make-tree-node 3))])
  (is-same-tree p q))

(let ([p (tree-node 1
                    (make-tree-node 2) #f)]
      [q (tree-node 1
                    #f (make-tree-node 2))])
  (is-same-tree p q))

(let ([p (tree-node 1
                    (make-tree-node 2) (make-tree-node 1))]
      [q (tree-node 1
                    (make-tree-node 1) (make-tree-node 2))])
  (is-same-tree p q))
