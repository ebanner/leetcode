#lang racket

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

(define/contract (inorder-traversal node)
  (-> (or/c tree-node? #f) (listof exact-integer?))

  (let/ec return
    (when (not node) (return '()))

    (define-values (left-nodes right-nodes)
      (values (inorder-traversal (tree-node-left node))
              (inorder-traversal (tree-node-right node))))

    (append left-nodes
            (list (tree-node-val node))
            right-nodes)))

(let ([tree (tree-node 1
                       #f (tree-node 2
                                     (make-tree-node 3) #f))])
  (inorder-traversal tree))
