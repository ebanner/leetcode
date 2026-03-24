#lang racket

(require racket/match)

(define ≤ <=)

(define-syntax-rule (define* (name args ...) body ...)
  (define name
    (match-lambda*
      [(list args ...)
       body ...])))

(define-syntax-rule (for*/max ([var seq] ...) body ...)
  (inexact->exact
   (for*/fold ([_max -inf.0]) ([var seq] ...)
     (max _max (let () body ...)))))

(define* (grid-ref grid [list i j])
  (vector-ref (list-ref grid i) j))

(define* (grid-set! grid [list i j] value)
  (vector-set! (list-ref grid i) j value))

(define* (safe? grid [list i j])
  (define-values (N M)
    (values (length grid)
            (vector-length (first grid))))
  (and
   (≤ 0
      i)
   (< i
      N)
   (≤ 0 j) (< j M)))

(define* (is-unexplored-island? grid [list i j])
  (positive? (grid-ref grid [list i j])))

(define (make-grid grid)
  (for/list ([row grid])
    (list->vector row)))

(define* (get-neighbors grid [list i j])
  (define neighbors
    (list [list (sub1 i) j]
          [list i (sub1 j)] [list i (add1 j)]
          [list (add1 i) j]))

  (for/set ([neighbor neighbors]
            #:when (and (safe? grid neighbor)
                        (is-unexplored-island? grid neighbor)))
    neighbor))

(define* (bfs grid [list i j])
  (let loop ([frontier (set [list i j])]
             [num-visited 0])

    (if (set-empty? frontier)
        num-visited
        (loop (for/fold ([acc (set)])
                        ([node frontier])
                (grid-set! grid node -1)
                (set-union (get-neighbors grid node) acc))
              (+ (set-count frontier) num-visited)))))

(define/contract (max-area-of-island grid*)
  (-> (listof (listof exact-integer?)) exact-integer?)

  (define-values (N M)
    (values (length grid*)
            (length (first grid*))))

  (define grid (make-grid grid*))

  (for*/max ([i N]
             [j M])

    (if (is-unexplored-island? grid [list i j])
        (bfs grid [list i j])
        0)))

(provide max-area-of-island)
