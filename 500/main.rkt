#lang racket

(define-syntax ~>
  (syntax-rules ()
    [(_ x) x]
    [(_ x f rest ...) (~> (f x) rest ...)]))

(define (string->set s)
  (~> s string->list list->set))

(define TOP-ROW (~> "QWERTYUIOP" string->set))
(define MIDDLE-ROW (~> "ASDFGHJKL" string->set))
(define BOTTOM-ROW (~> "ZXCVBNM" string->set))

(define (all-on-one? word)
  (or
   (for/and ([c (string-upcase word)])
     (set-member? MIDDLE-ROW c))
   (for/and ([c (string-upcase word)])
     (set-member? BOTTOM-ROW c))
   (for/and ([c (string-upcase word)])
     (set-member? TOP-ROW c))))

(define/contract (find-words words)
  (-> (listof string?) (listof string?))
  (for/list ([word words]
             #:when (all-on-one? word))
    word))

(provide find-words)
