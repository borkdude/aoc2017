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
          :when (zero? (mod greater smaller))]
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
  (part-2)

  ;; testing two different implementations of find-divisibles
  (defn find-divisibles-1 [nums]
    (let [state (atom 0)
          desc (sort-by - nums)
          asc  (sort nums)]
      (for [greater desc
            smaller asc
            :while (> greater smaller)
            :when (do
                    (swap! state inc)
                    (zero? (mod greater smaller)))]
        (do
          (println @state)
          [greater smaller]))))

  (defn find-divisibles-2 [nums]
    (let [state (atom 0)]
      (for [greater nums
            smaller nums
            :when (do (swap! state inc)
                      (and
                       (> greater smaller)
                       (zero? (mod greater smaller))))]
        (do
          (println @state)
          [greater smaller]))))

  (comment
    (def row-1 [409 194 207 470 178 454 235 333 511 103 474 293 525 372 408 428])
    (def row-2 [4321 2786 6683 3921 265 262 6206 2207 5712 214 6750 2742 777 5297 3764 16])
    (first (find-divisibles-1 row-1)) ;; 47 steps
    (first (find-divisibles-2 row-1)) ;; 55 steps
    (first (find-divisibles-1 row-2)) ;; 31 steps
    (first (find-divisibles-2 row-2)) ;; 106 steps
    ;; maybe find-divisibles-1 is worth the overhead of sorting? we'll go with that one for now
    )
  )
