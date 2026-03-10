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

(define (has-left-leaf node)
  (define left (tree-node-left node))

  (and left
       (not (tree-node-left left))
       (not (tree-node-right left))))

(define (get-left-leaf node)
  (tree-node-left node))

(define/contract (sum-of-left-leaves node)
  (-> (or/c tree-node? #f) exact-integer?)

  (let/ec return
    (when (not node)
      (return 0))

    (when (and (not (tree-node-left node))
               (not (tree-node-right node)))
      (return 0))

    (when (and (has-left-leaf node)
               (not (tree-node-right node)))
      (return (tree-node-val (get-left-leaf node))))

    (when (has-left-leaf node)
      (return (+ (tree-node-val (get-left-leaf node))
                 (sum-of-left-leaves (tree-node-right node)))))

    (+ (sum-of-left-leaves (tree-node-left node))
       (sum-of-left-leaves (tree-node-right node)))))

(let ([tree
       (tree-node 3
                  (make-tree-node 9)
                  (tree-node 20
                             (make-tree-node 15)
                             (make-tree-node 7)))])
  (sum-of-left-leaves tree))

(let ([tree (make-tree-node 1)])
  (sum-of-left-leaves tree))
