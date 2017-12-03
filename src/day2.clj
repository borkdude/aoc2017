(ns day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [util :refer [resource-reducible]]))

(defn part-1 []
  (transduce
   (comp
    (map #(str/split % #"\s"))
    (map #(map (fn [s]
                 (Integer/parseInt s))
               %))
    (map (fn [row]
           [(apply max row)
            (apply min row)]))
    (map (fn [[max min]]
           (- max min))))
   +
   (resource-reducible "day2.txt")))

(defn find-divisibles [nums]
  (let [desc (sort-by - nums)
        asc  (sort nums)]
    (for [greater desc
          smaller asc
          :while (> greater smaller)
          :when  (zero? (mod greater smaller))]
      [greater smaller])))

(defn part-2 []
  (transduce
   (comp
    (map #(str/split % #"\s"))
    (map #(map (fn [s]
                 (Integer/parseInt s))
               %))
    (map (fn [row]
           (first (find-divisibles row))))
    (map (fn [[greater smaller]]
           (/ greater smaller))))
   +
   (resource-reducible "day2.txt")))

;;;; Scratch

(comment
  (part-1)
  (part-2))
