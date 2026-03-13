#lang racket

(define ∞ (in-naturals))

(define (list->hash-vector nums)
  (for/hash ([(num i) (in-indexed nums)])
    (values i num)))

(define (hash-vector-ref nums i)
  (hash-ref nums i))

(define (hash-vector-ref-update nums i f)
  (hash-set nums i (f (hash-ref nums i))))

(define (get-jump-from-map nums-list)
  (define N (length nums-list))

  (for/fold ([jump-from-map (hash)]
             [nums (list->hash-vector nums-list)]
             [i 0]
             #:result jump-from-map)
            ([_ ∞]
             #:break (= i N))

    (values
     (let ([to (hash-vector-ref nums i)])
      (hash-update jump-from-map
                   (+ i to)
                   (λ (from) (if (positive? to) (set-add from i) from))
                   set))

     (if (positive? (hash-vector-ref nums i))
         (hash-vector-ref-update nums i sub1)
         nums)

     (if (> (hash-vector-ref nums i) 1)
         i
         (add1 i)))))

(define/contract (can-jump nums)
  (-> (listof exact-integer?) boolean?)

  (define N (length nums))

  (define jump-from-map (get-jump-from-map nums))

  (let/ec return
    (when (= N 1)
      (return #t))

    (for/fold ([frontier (hash-ref jump-from-map (sub1 N) (set))])
              ([_ ∞])

      (cond [(set-empty? frontier) (return #f)]
            [(set-member? frontier 0) (return #t)]
            [else (for*/set ([index frontier]
                             [to (hash-ref jump-from-map index (set))])
                    to)]))))

;; (can-jump '(2 3 1 1 4))

;; (can-jump '(3 2 1 0 4))

;; (can-jump '(1))

;; (can-jump '(0 2 3))

;; (can-jump '(1 0 1 0))
