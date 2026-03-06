#lang errortrace racket

(define (list-append lst x) (append lst (list x)))

(define (count sequence char)
  (for/sum ([c sequence]
            #:when (char=? c char))
    1))

(define (get-num char)
  (lambda (sequence) (count sequence char)))

(define/contract (generate-parenthesis n)
  (-> exact-integer? (listof string?))

  (define PAREN-SEQUENCES '())

  (let generate-all-sequences ([sequence '()])
    (let/ec return

      (define-values (num-open num-closed)
        (values ((get-num #\() sequence)
                ((get-num #\)) sequence)))

      (when (< num-open num-closed)
        (return))

      (when (or (> num-open n) (> num-closed n))
        (return))

      (when (and (= num-open n) (= num-closed n))
        (set! PAREN-SEQUENCES
              (list-append PAREN-SEQUENCES sequence))
        (return))

      (generate-all-sequences (list-append sequence #\())
      (generate-all-sequences (list-append sequence #\)))))

  (for/list ([sequence PAREN-SEQUENCES])
    (list->string sequence)))

(generate-parenthesis 3)
