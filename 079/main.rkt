#lang racket

(require racket/match
         racket/list)

(define ≤ <=)

(define-syntax-rule (define* (name args ...) body ...)
  (define name
    (match-lambda*
      [(list args ...)
       body ...])))

(define-syntax (for/fold* stx)
  (syntax-case stx ()
    [(_ (accs ...) ([var seq] ...) kw [c r] body ...)
     (equal? (syntax->datum #'kw) '#:break)
     #'(let/ec %break
         (for/fold (accs ...)
                   ([var seq] ...)
           (when c (%break r))
           body ...))]))

(define-syntax-rule (for*/first (clauses ...) body ...)
  (for*/or (clauses ...)
    body ...))

(define-syntax-rule (for/first* (clause ...) body ...)
  (for/first (clause ...) #t))

(define-syntax-rule (for*/first* (clause ...) body ...)
  (for*/first (clause ...) #t))

(define-syntax-rule (char≠? x y) (not (char=? x y)))

(define* (grid-ref grid [list i j])
  (hash-ref grid [list i j]))

(define* (grid-set grid [list i j] value)
  (hash-set grid [list i j] value))

(define (list->hash-grid board)
  (hash-set*
   (for*/hash ([(row i) (in-indexed board)]
               [(letter j) (in-indexed row)])
     (values [list i j] letter))

   'N (length board)
   'M (length (first board))))

(define (get-size grid)
  (values (hash-ref grid 'N) (hash-ref grid 'M)))

(define* (safe? grid [list i j])
  (define-values (N M)
    (get-size grid))

  (and
   (≤
    0
    i)
   (<
    i
    N)
   (≤ 0 j) (< j M)))

(define* (get-neighbors grid [list i j])
  (define candidates
    (list [list (sub1 i) j]
          [list i (sub1 j)] [list i (add1 j)]
          [list (add1 i) j]))

  (for/list ([candidate candidates]
             #:when (safe? grid candidate))
    candidate))

(define ZERO (lambda () 0))

(define (get-counts board)
  (for/fold ([counts (hash)])
            ([c (hash-values board)])

    (hash-update counts c add1 ZERO)))

(define (possible? board word)
  (for/fold* ([counts (get-counts board)]
              #:result #t)
             ([c word])
             #:break [(zero? (hash-ref counts c 0)) #f]

    (hash-update counts c sub1)))

(define/contract (exist board-list word)
  (-> (listof (listof char?)) string? boolean?)

  (define-values (N M)
    (values (length board-list)
            (length (first board-list))))

  (define board (list->hash-grid board-list))

  (define* (dfs [list i j] k board)
    (let/ec return
      (cond
        [(= k (string-length word)) (return #t)]
        [(char=? (grid-ref board [list i j]) #\.) (return #f)]
        [(char≠? (string-ref word k) (grid-ref board [list i j])) (return #f)])

      (for/first* ([neighbor (get-neighbors board [list i j])]
                   #:when (dfs neighbor
                               (add1 k)
                               (grid-set board [list i j] #\.))))))

  (let/ec return
    (when (not (possible? board word))
      (return #f))

    (when (equal? board-list (list (string->list word)))
      (return #t))

    (for*/first* ([i N]
                  [j M]
                  #:when (dfs (list i j) 0 board)))))

;; (let ([board '("ABCE"
;;                "SFCS"
;;                "ADEE")]
;;       [word "ABCCED"])

;;   (exist (for/list ([row board]) (for/list ([c row]) c))
;;          word))

;; (let ([board '(("A" "B" "C" "E")
;;                ("S" "F" "C" "S")
;;                ("A" "D" "E" "E"))]
;;       [word "SEE"])

;;   (exist (for/list ([row board]) (for/list ([s row]) (string-ref s 0)))
;;          word))

;; (let ([board '(("A" "B" "C" "E")
;;                ("S" "F" "C" "S")
;;                ("A" "D" "E" "E"))]
;;       [word "ABCB"])

;;   (exist (for/list ([row board]) (for/list ([s row]) (string-ref s 0)))
;;          word))

;; (let ([board '(("A"))]
;;       [word "A"])

;;   (exist (for/list ([row board]) (for/list ([s row]) (string-ref s 0)))
;;          word))

;; (let ([board '(("A" "B"))]
;;       [word "BA"])

;;   (exist (for/list ([row board]) (for/list ([s row]) (string-ref s 0)))
;;          word))

;; (let ([board '(("A" "B")
;;                ("C" "D"))]
;;       [word "ACDB"])

;;   (exist (for/list ([row board]) (for/list ([s row]) (string-ref s 0)))
;;          word))

;; (let ([board '(("A" "A" "A" "A" "A" "A")
;;                ("A" "A" "A" "A" "A" "A")
;;                ("A" "A" "A" "A" "A" "A")
;;                ("A" "A" "A" "A" "A" "A")
;;                ("A" "A" "A" "A" "A" "A")
;;                ("A" "A" "A" "A" "A" "A"))]
;;       [word "AAAAAAAAAAAAAAB"])

;;   (exist (for/list ([row board]) (for/list ([s row]) (string-ref s 0)))
;;          word))
