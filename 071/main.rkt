#lang racket

(define-syntax ~>
  (syntax-rules ()
    [(_ x) x]
    [(_ x f rest ...) (~> (f x) rest ...)]))

(define-syntax-rule (string≠? s t) (not (string=? s t)))

(define (remove-trailing-slashes path)
  (string-trim path "/" #:left? #f))

(define (dedupe-slashes path)
  (regexp-replace* #px"/+" path "/"))

(define (delete-dots path)
  (string-join
   (for/list ([dir (string-split path "/")]
              #:when (string≠? dir "."))
     dir)
   "/"))

(define (normalize-parents path)
  (for/fold ([stack '()]
             #:result (string-join (reverse stack) "/"))
            ([dir (string-split path "/")])

    (if (string=? dir "..")
        (if (pair? stack) (rest stack) stack)
        (cons dir stack))))

(define (prepend-slash path)
  (string-append "/" path))

(define/contract (simplify-path path)
  (-> string? string?)

  (~> path
      remove-trailing-slashes
      dedupe-slashes
      delete-dots
      normalize-parents
      prepend-slash))

;; (simplify-path "/home/")

;; (simplify-path "/home//foo/")

;; (simplify-path "/home/user/Documents/../Pictures")

;; (simplify-path "/../")

;; (simplify-path "/.../a/../b/c/../d/./")

;; (simplify-path "/a/./b/../../c/")
