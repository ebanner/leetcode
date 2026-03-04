#lang racket

(define-syntax-rule (list-append lst elem) (append lst (list elem)))

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

(define (format-path path)
(string-join (map number->string path) "->"))

(define/contract (binary-tree-paths root)
  (-> (or/c tree-node? #f) (listof string?))

  (define PATHS '())

  (let loop ([node root]
             [path '()])

    (let/ec return
      (when (not node) (return))

      (when (and (not (tree-node-left node))
                 (not (tree-node-right node)))
        (let ([full-path
               (list-append path (tree-node-val node))])
          (set! PATHS (list-append PATHS full-path)))
        (return))

      (loop (tree-node-left node)
            (append path (list (tree-node-val node))))

      (loop (tree-node-right node)
            (append path (list (tree-node-val node))))))

  (for/list ([path PATHS])
    (format-path path)))

(let ([tree (tree-node 1
                       (tree-node 2 #f (make-tree-node 5))
                       (make-tree-node 3))])
  (binary-tree-paths tree))
