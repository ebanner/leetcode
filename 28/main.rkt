#lang racket

(define/contract (str-str haystack needle)
  (-> string? string? exact-integer?)

  (or
   (string-find haystack needle)
   -1))

(str-str "sadbutsad" "sad")

(str-str "leetcode" "leeto")
