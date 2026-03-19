#lang racket

(require data/gvector)

(define-syntax ~>
  (syntax-rules ()
    [(_ x) x]
    [(_ x f rest ...) (~> (f x) rest ...)]))

(define (gvector-add vec x) (begin0 vec (gvector-add! vec x)))

(define (delete-dashes s)
  (string-replace s "-" ""))

(define (get-group-1-size dashless-s k)
  (remainder (string-length dashless-s) k))

(define (group s k i)
  (let loop ([groups (make-gvector)]
             [s (substring s i)])

    (if (string=? s "")
        (gvector->list groups)
        (loop (gvector-add groups (substring s 0 k))
              (substring s k)))))

(define/contract (license-key-formatting s k)
  (-> string? exact-integer? string?)

  (define dashless-s (~> s string-upcase delete-dashes))
  (define group-1-size (get-group-1-size dashless-s k))
  (define group-1 (substring dashless-s 0 group-1-size))

  (define groups
    (let ([rest-groups (group dashless-s k group-1-size)])
     (if (positive? group-1-size)
         (cons group-1 rest-groups)
         rest-groups)))

  (string-join groups "-"))

;; (license-key-formatting "5F3Z-2e-9-w" 4)

;; (license-key-formatting "2-5g-3-J" 2)

;; (license-key-formatting "2-4A0r7-4k" 3)
