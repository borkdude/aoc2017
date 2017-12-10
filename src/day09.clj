(ns day09
  (:require day09-regex
            day09-single-pass
            day09-instaparse))

(defn part-1 []
  [(day09-regex/part-1)
   (day09-single-pass/part-1)
   (day09-instaparse/part-1)])

(defn part-2 []
  [(day09-regex/part-2)
   (day09-single-pass/part-2)
   (day09-instaparse/part-2)])
