(ns day09-single-pass
  (:require
   [criterium.core :refer [quick-bench]]
   [util :refer [read-first]]))

(def lcb \{)
(def rcb \})
(def lab \<)
(def rab \>)
(def exl \!)

(defn data
  []
  (read-first "day9.txt"))

(defn rf
  [{:keys [level score gc
           ignore? in-garbage?]
    :as state} c]
  (cond
    ignore?
    (assoc state :ignore? false)
    ;; else
    in-garbage?
    (cond
      (= exl c)
      (assoc state :ignore? true)
      ;; else
      (= rab c)
      (assoc state :in-garbage? false)
      ;; else
      :else
      (update state :gc inc))
    ;; else
    (= lab c)
    (assoc state :in-garbage? true)
    ;; else
    (= lcb c)
    (assoc state
           :level (inc level)
           :score (+ score level))
    ;; else
    (= rcb c)
    (update state :level dec)
    :else state))

(defn solve
  [chars]
  (reduce rf {:level 1
              :score 0
              :gc 0
              :ignore? false
              :in-garbage? false
              :pos 0}
          chars))

(defn part-1
  []
  (-> (data) solve :score))

(defn part-2
  []
  (-> (data) solve :gc))

;;;; Scratch

(comment
  (part-1) ;; 20530, fellshard: 17390, bhauman: 13154
  (part-2) ;; 9978,  fellshard: 7825,  bhauman: 6369
  )
