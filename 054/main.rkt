#lang errortrace racket

(require racket/match
         racket/generator)

(define-syntax-rule (define* (name args ...) body ...)
  (define name
    (match-lambda*
      [(list args ...)
       body ...])))

(define* (grid-set! matrix (list i j) value)
  (vector-set! (vector-ref matrix i) j value))

(define* (grid-ref matrix (list i j))
  (vector-ref (vector-ref matrix i) j))

(define (list-append lst x) (append lst (list x)))

(define (safe-index matrix i j)
  (define-values (N M)
    (values (vector-length matrix)
            (vector-length (vector-ref matrix 0))))

  (and (<= 0 i)
       (<= 0 j) (< j M)
       (< i N)
       (grid-ref matrix [list i j])))

(define (in-singleton-rows matrix)
  (in-generator
   (for ([row matrix])
     (yield (vector-ref row 0)))))

(define* (get-next matrix (list i j) direction)
  (define-values (N M)
    (values (vector-length matrix)
            (vector-length (vector-ref matrix 0))))

  (let/ec return
    (when (and (eq? direction 'E)
               (safe-index matrix i (add1 j)))
      (return (list i (add1 j)) 'E))

    (when (and (eq? direction 'E)
               (not (safe-index matrix i (add1 j))))
      (return (list (add1 i) j) 'S))

    (when (and (eq? direction 'S)
               (safe-index matrix (add1 i) j))
      (return (list (add1 i) j) 'S))

    (when (and (eq? direction 'S)
               (not (safe-index matrix (add1 i) j)))
      (return (list i (sub1 j)) 'W))

    (when (and (eq? direction 'W)
               (safe-index matrix i (sub1 j)))
      (return (list i (sub1 j)) 'W))

    (when (and (eq? direction 'W)
               (not (safe-index matrix i (sub1 j))))
      (return (list (sub1 i) j) 'N))

    (when (and (eq? direction 'N)
               (safe-index matrix (sub1 i) j))
      (return (list (sub1 i) j) 'N))

    (when (and (eq? direction 'N)
               (not (safe-index matrix (sub1 i) j)))
      (return (list i (add1 j)) 'E))

    (values #f direction)))

(define/contract (spiral-order matrix-list)
  (-> (listof (listof exact-integer?)) (listof exact-integer?))

  (define matrix
    (for/vector ([row matrix-list])
      (list->vector row)))

  (define-values (N M)
    (values (vector-length matrix)
            (vector-length (vector-ref matrix 0))))

  (let/ec return
    (when (= N 1)
      (return (vector->list (vector-ref matrix 0))))

    (when (= M 1)
      (return
       (for/list ([(elem) (in-singleton-rows matrix)])
         elem)))

    (let loop ([position '(0 0)]
               [direction 'E]
               [acc '()])

      (when (not (grid-ref matrix position))
        (return acc))

      (define value (grid-ref matrix position))

      (grid-set! matrix position #f)

      (define-values (next-position next-direction)
        (get-next matrix position direction))

      (if next-position
          (loop next-position
                next-direction
                (list-append acc value))
          acc))))

(let ([matrix '((1 2 3)
                (4 5 6)
                (7 8 9))])
  (spiral-order matrix))

(let ([matrix '((1 2 3 4)
                (5 6 7 8)
                (9 10 11 12))])
  (spiral-order matrix))


(let ([matrix '((1))])
  (spiral-order matrix))

(let ([matrix '((3) (2))])
  (spiral-order matrix))

