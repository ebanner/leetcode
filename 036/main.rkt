#lang errortrace racket

(require racket/generator)

(define-syntax-rule (add3 x) (add1 (add1 (add1 x))))

(define (slice lst start end)
  (take (drop lst start) (- end start)))

(define (number->char n)
  (integer->char (+ n (char->integer #\0))))

(define (grid-ref grid i j)
  (list-ref (list-ref grid i) j))

(define (in-singles xs)
  (in-generator
   (for ([t xs])
     (yield (first t)))))

(define (in-rows board)
  (in-generator
   (for ([row board])
     (yield row))))

(define (row-is-valid? row)
  (let/ec return
    (for/fold ([nums (list->set
                      (map number->char (range 10)))])
              ([num row])
      (let/ec continue
        (when (char=? num #\.)
          (continue nums))

        (if (not (set-member? nums num))
            (return #f)
            (set-remove nums num))))
    #t))

(define (in-columns board)
  (in-generator
   (for ([j 9])
     (define column
       (for/list ([i 9]) (list (grid-ref board i j))))
     (yield column))))

(define (column-is-valid? column)
  (let/ec return
   (for/fold ([nums (list->set
                     (map number->char (range 10)))])
             ([(num) (in-singles column)])
     (let/ec continue
       (when (char=? num #\.)
         (continue nums))

       (if (not (set-member? nums num))
           (return #f)
           (set-remove nums num))))

    #t))

(define (get-grid board start-row start-column)
  (for/list ([i (in-range start-row (add3 start-row))])
    (slice (list-ref board i)
           start-column
           (add3 start-column))))

(define (in-grids board)
  (in-generator
   (for* ([i (in-range 0 (add1 6) 3)]
          [j (in-range 0 (add1 6) 3)])
     (yield (get-grid board i j)))))

(define (grid-is-valid? grid)
  (let/ec return
   (for*/fold ([nums (list->set
                      (map number->char (range 10)))])
              ([i 3]
               [j 3])
     (define num (grid-ref grid i j))

     (let/ec continue
       (when (char=? num #\.)
         (continue nums))

       (if (not (set-member? nums num))
           (return #f)
           (set-remove nums num))))

    #t))

(define/contract (is-valid-sudoku board)
  (-> (listof (listof char?)) boolean?)

  (and
   (for/and ([row (in-rows board)])
     (row-is-valid? row))

   (for/and ([column (in-columns board)])
     (column-is-valid? column))

   (for/and ([grid (in-grids board)])
     (grid-is-valid? grid))))

(let ([grid '(("5" "3" "." "." "7" "." "." "." ".")
              ("6" "." "." "1" "9" "5" "." "." ".")
              ("." "9" "8" "." "." "." "." "6" ".")
              ("8" "." "." "." "6" "." "." "." "3")
              ("4" "." "." "8" "." "3" "." "." "1")
              ("7" "." "." "." "2" "." "." "." "6")
              ("." "6" "." "." "." "." "2" "8" ".")
              ("." "." "." "4" "1" "9" "." "." "5")
              ("." "." "." "." "8" "." "." "7" "9"))])

  (is-valid-sudoku
   (for/list ([row grid])
     (for/list ([cell row]) (string-ref cell 0)))))
