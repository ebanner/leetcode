#lang errortrace racket

(define-syntax for/string
  (syntax-rules ()
    [(_ clauses body ...)
     (list->string
      (for/list clauses body ...))]))

(define (get-grid num-rows)
  (build-vector num-rows (λ (_) (make-vector 1000 0))))

(define (grid-set! grid i j elem)
  (vector-set! (vector-ref grid i) j elem))

(define (get-next-indices i j num-rows direction)
  (let/ec return
    (when (and (eq? direction 'S) (= i (sub1 num-rows)))
      (return (sub1 i) (add1 j) 'NE))

    (when (and (eq? direction 'NE) (zero? i))
      (return (add1 i) j 'S))

    (when (eq? direction 'NE)
      (return (sub1 i) (add1 j) 'NE))

    (when (eq? direction 'S)
      (return (add1 i) j 'S))))

(define (fill-grid grid s num-rows)
  (for/fold ([i 0]
             [j 0]
             [direction 'S])
            ([c s])

    (grid-set! grid i j c)
    (get-next-indices i j num-rows direction))

  grid)

(define (get-string filled-grid)
  (define (get-string- row)
    (for/string ([elem row]
                 #:when (char? elem))
      elem))

  (define strings
    (for/list ([row filled-grid])
      (get-string- row)))

  (string-append* strings))

(define/contract (convert s num-rows)
  (-> string? exact-integer? string?)

  (let/ec return
    (when (= num-rows 1) (return s))

    (define grid (get-grid num-rows))
    (define filled-grid (fill-grid grid s num-rows))
    (define string (get-string filled-grid))

    string))

(convert "AB" 1)
