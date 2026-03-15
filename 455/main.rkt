#lang errortrace racket

(define ≤ <=)
(define ≥ >=)

(define-syntax (for/fold* stx)
  (syntax-case stx ()
    [(_ (accs ...) ([var seq] ...) kw [c r] body ...)
     (equal? (syntax->datum #'kw) '#:break)
     #'(let/ec %break
         (for/fold (accs ...)
                   ([var seq] ...)
           (when c (%break r))
           body ...))]))

(define/contract (find-content-children g s)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)

  (define-values (G S)
    (values (list->vector (sort g <))
            (list->vector (sort s <))))

  (define-values (N M)
    (values (vector-length G)
            (vector-length S)))

  (for/fold* ([num-cookies 0]
              [i 0]
              #:result num-cookies)
             ([j M])
             #:break ((≥ i N) num-cookies)

    (if (≤ (vector-ref G i) (vector-ref S j))
        (values (add1 num-cookies) (add1 i))
        (values num-cookies i))))

;; (find-content-children '(1 2 3) '(1 1))

;; (find-content-children '(1 2) '(1 2 3))

;; (find-content-children '(1 2 3) '())

;; (find-content-children '(10 9 8 7) '(5 6 7 8))
