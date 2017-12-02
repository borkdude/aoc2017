(ns day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [util :refer [input]]))

(defn part-1 []
  (let [txt (input "day1.txt")]
    (reduce
     +
     (map
      (fn [a b]
        (if (= a b)
          (Integer/parseInt (str a)) 0))
      txt
      (drop 1 (cycle txt))))))

(defn part-2 []
  (let [txt (input "day1.txt")
        half (/ (count txt) 2)]
    (reduce
     +
     (map
      (fn [a b]
        (if (= a b)
          (Integer/parseInt (str a)) 0))
      txt
      (drop half (cycle txt))))))
