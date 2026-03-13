#lang errortrace racket

(require racket/match
         racket/generator)

(define-syntax-rule (define* (name args ...) body ...)
  (define name
    (match-lambda*
      [(list args ...)
       body ...])))

(define (in-pairs xs)
  (make-do-sequence
   (λ ()
     (define (pos->vals s)
       (let ([t (car s)])
         (values (car t) (cadr t))))
     (values pos->vals cdr xs pair? #f #f))))

(define* (grid-set matrix [list i j] value)
  (hash-set matrix [list i j] value))

(define* (grid-ref matrix [list i j])
  (hash-ref matrix [list i j]))

(define (list-append lst x) (append lst (list x)))

(define (safe-index matrix i j)
  (define-values (N M)
    (get-size matrix))

  (and (<= 0 i)
       (<= 0 j) (< j M)
       (< i N)
       (grid-ref matrix [list i j])))

(define (in-singleton-rows matrix)
  (in-generator
   (for ([row matrix])
     (yield (first row)))))

(define* (get-next matrix [list i j] direction)
  (define-values (N M)
    (get-size matrix))

  (let/ec return
    (when (and (eq? direction 'E)
               (safe-index matrix i (add1 j)))
      (return (list i (add1 j)) 'E))

    (when (and (eq? direction 'E)
               (not (safe-index matrix i (add1 j))))
      (return (list (add1 i) j) 'S))

    (when (and (eq? direction 'S)
               (safe-index matrix (add1 i) j))
      (return (list (add1 i) j) 'S))

    (when (and (eq? direction 'S)
               (not (safe-index matrix (add1 i) j)))
      (return (list i (sub1 j)) 'W))

    (when (and (eq? direction 'W)
               (safe-index matrix i (sub1 j)))
      (return (list i (sub1 j)) 'W))

    (when (and (eq? direction 'W)
               (not (safe-index matrix i (sub1 j))))
      (return (list (sub1 i) j) 'N))

    (when (and (eq? direction 'N)
               (safe-index matrix (sub1 i) j))
      (return (list (sub1 i) j) 'N))

    (when (and (eq? direction 'N)
               (not (safe-index matrix (sub1 i) j)))
      (return (list i (add1 j)) 'E))

    (values #f direction)))

(define (list->hash-grid matrix)
  (define-values (N M)
    (values (length matrix)
            (length (first matrix))))

  (for*/hash ([i N]
              [j M])

    (values [list i j]
            (list-ref (list-ref matrix i) j))))

(define (get-size grid)
  (define-values (N M)
    (values (add1
             (apply max
              (for/list ([(i _) (in-pairs (hash-keys grid))]) i)))
            (add1
             (apply max
              (for/list ([(_ j) (in-pairs (hash-keys grid))]) j)))))

  (values N M))

(define (get-data grid)
  (define-values (N M)
    (get-size grid))

  (define data
   (for/vector ([_ N])
     (make-vector M 0)))

  (for* ([i N] [j M])
    (vector-set! (vector-ref data i) j (hash-ref grid [list i j])))

  (for/list ([row data])
    (vector->list row)))

(define/contract (spiral-order matrix-list)
  (-> (listof (listof exact-integer?)) (listof exact-integer?))

  (define-values (N M)
    (values (length matrix-list)
            (length (first matrix-list))))

  (let/ec return
    (when (= N 1)
      (return (first matrix-list)))

    (when (= M 1)
      (return
       (for/list ([(elem) (in-singleton-rows matrix-list)])
         elem)))

    (let loop ([path '()]
               [matrix (list->hash-grid matrix-list)]
               [position '(0 0)]
               [direction 'E])

      (when (not (grid-ref matrix position))
        (return path))

      (define-values (next-position next-direction)
        (get-next matrix position direction))

      (if next-position
          (loop (list-append path (grid-ref matrix position))
                (grid-set matrix position #f)
                next-position
                next-direction)
          path))))

(let ([matrix '((1 2 3)
                (4 5 6)
                (7 8 9))])
  (spiral-order matrix))

;; (let ([matrix '((1 2 3 4)
;;                 (5 6 7 8)
;;                 (9 10 11 12))])
;;   (spiral-order matrix))

;; (let ([matrix '((1))])
;;   (spiral-order matrix))

;; (let ([matrix '((3) (2))])
;;   (spiral-order matrix))

;; (let ([matrix '((1 2 3 4)
;;                 (5 6 7 8)
;;                 (9 10 11 12))])
;;   (spiral-order matrix))

