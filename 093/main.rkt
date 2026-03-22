#lang racket

(require srfi/13)

(define-syntax-rule (≠ x y) (not (= x y)))
(define ≤ <=)

(define (list-append lst x) (append lst (list x)))

(define (make-ip-string s dots)
  (for/fold ([ip-string '()]
             #:result (list->string ip-string))
            ([(c i) (in-indexed s)])

    (if (not (member i dots))
        (append ip-string (list c))
        (append ip-string (list c) (list #\.)))))

(define (valid-ip? ip-string)
  (let/ec return
    (define num-dots (string-count ip-string #\.))

    (when (≠ num-dots 3) (return #f))

    (define chunks (string-split ip-string "."))

    (when (< (length chunks) 4) (return #f))

    (for ([chunk chunks])
      (when (and (string-prefix? "0" chunk)
                 (> (string-length chunk) 1))
        (return #f)))

    (for/and ([chunk chunks])
      (and
       (string->number chunk)
       (let ([num (string->number chunk)])
         (and (≤ 0 num)) (≤ num 255))))))

(define (valid-prefix? ip-string)
  (let/ec return
   (for ([chunk (string-split ip-string ".")])
     (when (and (string-prefix? "0" chunk) (> (string-length chunk) 1))
       (return #f)))

    (define num-dots (string-count ip-string #\.))

    (and
     (≤ num-dots 3)
     (for/and ([chunk (string-split ip-string ".")])
       (and
        (string->number chunk)
        (let ([num (string->number chunk)])
          (and (≤ 0 num)) (≤ num 255)))))))

(define/contract (restore-ip-addresses s)
  (-> string? (listof string?))

  (define N (string-length s))
  (define IP-STRINGS '())

  (let loop ([dots '()]
             [i 0])

    (let/ec return
      (when (= i N)
        (when (valid-ip? (make-ip-string s dots))
          (set! IP-STRINGS
                (list-append IP-STRINGS (make-ip-string s dots))))
        (return))

      (when (not (valid-prefix? (make-ip-string (substring s 0 i) dots)))
        (return))

      (loop (list-append dots i) (add1 i))
      (loop dots (add1 i))))

  IP-STRINGS)

;; (restore-ip-addresses "25525511135")

;; (restore-ip-addresses "0000")

;; (restore-ip-addresses "101023")

