#lang racket

(require racket/match)

(define-syntax-rule (define* (name args ...) body ...)
  (define name
    (match-lambda*
      [(list args ...)
       body ...])))

(define (get-dp triangle)
  (for/list ([row triangle])
    (make-vector (vector-length row) +inf.0)))

(define (triangle-list->triangle-vector triangle-list)
  (for/list ([row triangle-list])
    (list->vector row)))

(define* (triangle-ref triangle [list i j])
  (vector-ref (list-ref triangle i) j))

(define* (triangle-set! triangle [list i j] value)
  (vector-set! (list-ref triangle i) j value))

(define/contract (minimum-total triangle-list)
  (-> (listof (listof exact-integer?)) exact-integer?)

  (define triangle (triangle-list->triangle-vector triangle-list))

  (define dp (get-dp triangle))

  (triangle-set! dp [list 0 0]
                 (triangle-ref triangle [list 0 0]))

  (for* ([(row i) (in-indexed (drop-right dp 1))]
         [j (vector-length row)])

    (when (< (+ (triangle-ref dp [list i j])
                (triangle-ref triangle [list (add1 i) j]))
             (triangle-ref dp [list (add1 i) j]))

      (triangle-set! dp [list (add1 i) j]
                     (+ (triangle-ref dp [list i j])
                        (triangle-ref triangle [list (add1 i) j]))))

    (when (< (+ (triangle-ref dp [list i j])
                (triangle-ref triangle [list (add1 i) (add1 j)]))
             (triangle-ref dp [list (add1 i) (add1 j)]))

      (triangle-set! dp [list (add1 i) (add1 j)]
                     (+ (triangle-ref dp [list i j])
                        (triangle-ref triangle [list (add1 i) (add1 j)])))))

  (apply min (vector->list (last dp))))

(minimum-total '((2) (3 4) (6 5 7) (4 1 8 3)))

(minimum-total '((-10)))
