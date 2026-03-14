#lang errortrace racket

(require racket/match)

(define ≥ >=)

(define-syntax-rule (define* (name args ...) body ...)
  (define name
    (match-lambda*
      [(list args ...)
       body ...])))

(define (get-grid n m)
  (for/vector ([_ n])
    (make-vector m 0)))

(define* (grid-set! grid [list i j] result)
  (vector-set! (vector-ref grid i) j result))

(define* (grid-ref grid [list i j])
  (vector-ref (vector-ref grid i) j))

(define/contract (unique-paths n m)
  (-> exact-integer? exact-integer? exact-integer?)

  (define grid (get-grid n m))

  (grid-set! grid [list (sub1 n) (sub1 m)] 1)

  (let get-unique-paths ([i 0]
                         [j 0])
    (let/ec return
      (when (or (≥ i n) (≥ j m))
        (return 0))

      (when (positive? (grid-ref grid [list i j]))
        (return (grid-ref grid [list i j])))

      (define result (+ (get-unique-paths i (add1 j))
                        (get-unique-paths (add1 i) j)))

      (grid-set! grid [list i j] result)

      result))

  (grid-ref grid [list 0 0]))

;; (unique-paths 3 2)
