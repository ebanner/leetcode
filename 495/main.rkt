#lang racket

(define/contract (find-poisoned-duration time-series-list duration)
  (-> (listof exact-integer?) exact-integer? exact-integer?)

  (let/ec return
    (when (zero? duration)
      (return 0))

    (define time-series (list->set time-series-list))

    (for/fold ([seconds-poisoned 0]
               [seconds-remaining 0]
               #:result seconds-poisoned)
              ([t (+ (apply max time-series-list)
                     duration)])

      (if (set-member? time-series t)
          (values (add1 seconds-poisoned)
                  (sub1 duration))

          (values (if (positive? seconds-remaining)
                      (add1 seconds-poisoned)
                      seconds-poisoned)
                  (if (positive? seconds-remaining)
                      (sub1 seconds-remaining)
                      0))))))

;; (find-poisoned-duration '(1 4) 2)

;; (find-poisoned-duration '(1 2) 2)
