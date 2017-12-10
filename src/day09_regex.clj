(ns day09-regex
  (:require
   [clojure.string :as str]
   [criterium.core :refer [quick-bench]]
   [util :refer [read-first]]))

(def lcb \{)
(def rcb \})

(defn data
  []
  (read-first "day9.txt"))

(defn ignore
  [s]
  (str/replace s #"!." ""))

(def garbage-reg #"<(.*?)>")

(defn strip-garbage
  [s]
  (str/replace s garbage-reg ""))

(defn count-groups
  [s]
  (first
   (reduce (fn [[score level] c]
             (condp = c
               lcb [(+ score level)
                    (inc level)]
               rcb [score
                    (dec level)]
               [score level]))
           [0 1]
           s)))

(defn part-1
  []
  (-> (data)
      ignore
      strip-garbage
      count-groups))

(defn count-garbage
  [s]
  (->> s
       (re-seq garbage-reg)
       (map second)
       (map count)
       (apply +)))

(defn part-2
  []
  (-> (data)
      ignore
      count-garbage))

;;;; Scratch

(comment
  (part-1) ;; 20530
  (part-2) ;; 9978
  )
