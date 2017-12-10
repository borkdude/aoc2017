(ns day06
  (:require
   [clojure.string :as str]
   [criterium.core :refer [quick-bench]]
   [util :refer [read-first]])
  (:import [java.lang Math]))

(defn data []
  (as-> "day6.txt" $
    (read-first $)
    (str/split $ #"\s")
    (mapv #(Integer/parseInt %) $)))

(defn first-max-pos
  [nums]
  (reduce (fn [[_ max-val :as m]
               [_ cur-val :as c]]
            (if (> cur-val max-val)
              c m))
          (map vector (range) nums)))

(defn next-pos
  [state cur-pos]
  (mod (inc cur-pos) (count state)))

(defn next-state
  [state]
  (let [[max-pos max-val] (first-max-pos state)
        dist-size (->
                   (/ max-val (count state))
                   Math/ceil
                   int)]
    (loop [state state
           pos (next-pos state max-pos)]
      (let [left (get state max-pos)]
        (if (or
             (zero? left)
             (= pos max-pos))
          state
          (recur
           (->
            state
            (update pos + dist-size)
            (update max-pos - dist-size))
           (next-pos state pos)))))))

(defn solve
  [data]
  (loop [states #{data}
         state data
         n 1]
    (let [state' (next-state state)]
      (if (contains? states state')
        [state' n]
        (recur (conj states state')
               state'
               (inc n))))))

(defn part-1 []
  (second (solve (data))))

(defn part-2 []
  (second (solve (first (solve (data))))))

;;;; Scratch

(comment
  (quick-bench (part-1)) ;; 63 ms
  (quick-bench (part-2)) ;; 87 ms
  )

