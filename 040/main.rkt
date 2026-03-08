#lang racket

(define (grid-ref grid i j)
  (vector-ref (vector-ref grid i) j))

(define (grid-set! grid i j value)
  (vector-set! (vector-ref grid i) j value))

(define (grid+=! grid i j val)
  (define N (vector-length grid))

  (when (< i N)
    (vector-set! (vector-ref grid i)
                 j
                 (+ val
                    (grid-ref grid i j)))))

(define (get-dp candidates target)
  (define-values (N M)
    (values (add1 target)
            (vector-length candidates)))

  (define dp (build-vector N (λ (_) (make-vector M 0))))

  (grid-set! dp 0 0 1)
  (grid-set! dp (vector-ref candidates 0) 0 1)

  (define DP
    (build-vector
     N
     (λ (_) (build-vector M (λ (_) (mutable-set))))))

  (set-add! (grid-ref DP 0 0)
            '())

  (set-add! (grid-ref DP (vector-ref candidates 0) 0)
            (list (vector-ref candidates 0)))

  (values dp DP))

(define/contract (combination-sum2 candidates-list target)
  (-> (listof exact-integer?) exact-integer?
      (listof (listof exact-integer?)))

  (let/ec return
    (define candidates
      (for/list ([candidate candidates-list]
                 #:when (<= candidate target))
        candidate))

    (when (empty? candidates)
      (return '()))

    (define-values (N M)
      (values (add1 target)
              (length candidates)))

    (define nums (list->vector (sort candidates <)))

    (define-values (dp DP)
      (get-dp nums target))

    (for ([j (sub1 M)])
      (for ([i N])
        (let ([I (+ i
                    (vector-ref nums (add1 j)))]
              [J (add1 j)])

          (grid+=! dp I J (grid-ref dp i j)) ; add

          (for ([path (grid-ref DP i j)])
            (when (< I N)
              (set-add! (grid-ref DP I J)
                        (append path (list (vector-ref nums (add1 j))))))))

        (grid+=! dp                     ; don't add
                 i
                 (add1 j)
                 (grid-ref dp i j))

        (for ([path (grid-ref DP i j)])
          (set-add! (grid-ref DP i (add1 j)) path))))

    (set->list
     (grid-ref DP target (sub1 M)))))

(combination-sum2 '(2) 1)
