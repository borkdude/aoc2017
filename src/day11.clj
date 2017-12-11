(ns day11
  (:require
   [clojure.string :as str]
   [util :refer [read-first]])
  (:import [java.lang Math]))

(defn data
  []
  (as-> "day11.txt" $
    (read-first $)
    (str/split $ #",")))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn distance ^long
  [coords]
  (/ ^long (apply +
                  (map
                   #(Math/abs ^long
                              %)
                   coords))
     2))

(defn solve
  "See https://twitter.com/EsthervdSHU/status/940160750303268864/photo/1"
  [input]
  (reduce
   (fn [[[^long x ^long y ^long z]
         ^long max-dist] next-dir]
     (let [next-coords
           (case next-dir
             "n"  [(inc x) (inc y) z]
             "s"  [(dec x) (dec y) z]
             "ne" [(inc x) y (inc z)]
             "sw" [(dec x) y (dec z)]
             "nw" [x (inc y) (dec z)]
             "se" [x (dec y) (inc z)])
           next-max (max max-dist
                         (distance next-coords))]
       [next-coords next-max]))
   [[0 0 0] 0]
   input))

(defn part-1
  []
  (distance (first (solve (data)))))

(defn part-2
  []
  (second (solve (data))))

;;;; Scratch

(apply + (map #(Math/abs ^long %) [1 2 3]))

(comment
  (time (part-1)) ;; 794
  (time (part-2)) ;; 1524
  
)
