#lang racket

(require racket/generator)

(define-syntax-rule (% x) (remainder x 10))
(define-syntax-rule (>> x) (quotient x 10))
(define-syntax-rule (≠ x y) (not (= x y)))

(define (half N) (quotient N 2))

(define (digits-generator n)
  (generator ()
             (let loop ([n n])
               (when (positive? n)
                 (yield (% n))
                 (loop (>> n))))))

(define (in-digits n)
  (in-generator
   (let loop ([n n])
     (when (positive? n)
       (yield (% n))
       (loop (>> n))))))

(define (get-length x)
  (for/sum ([_ (in-digits x)]) 1))

(define (second-half x)
  (define N (get-length x))

  (define g (digits-generator x))

  (in-generator
   (for ([_ (in-range (half N))])
     (yield (g)))))

(define (first-half x)
  (define N (get-length x))

  (define g (digits-generator x))

  (for ([_ (in-range (half N))])
    (g))

  (when (odd? N)
    (g))

  (in-generator
   (for ([_ (in-range (half N))])
     (yield (g)))))

(define/contract (is-palindrome x)
  (-> exact-integer? boolean?)

  (let/ec return
    (when (negative? x)
      (return #f))

    (define stack
      (for/fold ([stack '()])
                ([digit (second-half x)])
        (cons digit stack)))

    (for/fold ([stack stack])
              ([digit (first-half x)])
      (when (≠ (first stack) digit)
        (return #f))

      (rest stack))

    #t))

(is-palindrome 3221)
