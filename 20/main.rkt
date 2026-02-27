#lang racket

(define-syntax-rule (char≠? x y) (not (char=? x y)))

(define/contract (is-valid s)
  (-> string? boolean?)

  (let/ec return
    (define stack
     (for/fold ([stack '()])
               ([c s])

       (when (and (or (char=? c #\)) (char=? c #\]) (char=? c #\}))
                  (empty? stack))

         (return #f))

       (when (or
              (and (char=? c #\)) (char≠? (first stack) #\())
              (and (char=? c #\]) (char≠? (first stack) #\[))
              (and (char=? c #\}) (char≠? (first stack) #\{)))

         (return #f))

       (if (or (char=? c #\() (char=? c #\[) (char=? c #\{))
           (cons c stack)
           (rest stack))))

    (empty? stack)))
