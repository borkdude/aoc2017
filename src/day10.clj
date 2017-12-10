(ns day10
  (:require
   [clojure.string :as str]
   [util :refer [read]])
  (:import [java.lang Math]))

(defn data
  []
  (as-> "day10.txt" $
    (read $)
    (str/split $ #",")
    (mapv #(Integer/parseInt %) $)))

(defn data-part-2
  []
  (as-> "day10.txt" $
    (read $)
    (mapv byte $)
    (into $ [17, 31, 73, 47, 23])))

(defn init-nums
  [] (into [] (range 0 256)))

(defn transform-nums
  [nums pos l]
  (let [reverse-positions (map #(mod % (count nums))
                               (range pos (+ pos l)))
        mapping (zipmap reverse-positions
                        (reverse reverse-positions))]
    (mapv (fn [pos' v]
            (get nums (get mapping pos' pos')))
          (range)
          nums)))

(defn solve
  [input init]
  (reduce
   (fn [[nums pos skip-size] l]
     [(transform-nums nums pos l)
      (+ pos l skip-size)
      (inc skip-size)])
   init
   input))

(defn part-1
  []
  (let [[nums _ _]
        (solve (data)
               [(init-nums) 0 0])]
    (apply * (take 2 nums))))

(defn part-2
  []
  (let [[sparse-hash _ _]
        (nth (iterate (partial solve (data-part-2))
                      [(init-nums) 0 0])
             64)
        dense-hash (map #(apply bit-xor %)
                        (partition 16 sparse-hash))
        hexed (apply str (map #(format "%x" %) dense-hash))]
    hexed))

;;;; Scratch

(comment
  (part-1) ;; 19591
  (part-2) ;; "62e2204d2ca4f4924f6e7a80f1288786"
  )
