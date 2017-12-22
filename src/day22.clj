(ns day22
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [util :refer [read-lines]]))

(defn start
  []
  (let [lines (read-lines "day22.txt")
        size  (count lines)
        center (quot size 2)
        positions (for [y (range 0 size)
                        x (range 0 size)]
                    [[x y] (get-in lines [y x])])
        grid (into {} positions)]
    {:grid grid
     :infected 0
     :direction :north
     :pos [center center]}))

(def left
  {:north :west
   :east  :north
   :south :east
   :west  :south})

(def right
  {:north :east
   :east  :south
   :south :west
   :west  :north})

(def reversed
  {:north :south
   :east  :west
   :south :north
   :west  :east})

(defn next-direction-1
  [current-direction infected?]
  (get (if infected? right left)
       current-direction))

(defn next-position
  [[x y] direction]
  (case direction
    :north [x (dec y)]
    :south [x (inc y)]
    :west  [(dec x) y]
    :east  [(inc x) y]))

(defn infected?
  [c]
  (= \# c))

(defn next-char-1
  [c]
  (if (= \# c)
    \. \#))

(defn next-state-1
  [{:keys [grid infected pos direction]}]
  (let [current (get grid pos)
        next-dir (next-direction-1 direction
                                   (infected? current))
        next-val (next-char-1 current)
        next-infected (if (infected? next-val)
                        (inc infected)
                        infected)
        next-grid (assoc grid pos next-val)
        next-pos (next-position pos next-dir)]
    {:grid next-grid
     :infected next-infected
     :pos next-pos
     :direction next-dir}))

(defn part-1
  []
  (:infected (nth (iterate next-state-1 (start)) 10000)))

(defn next-char-2
  [c]
  (case c
    \F \.
    \# \F
    \W \#
    ;; \. or nil
    \W))

(defn next-direction-2
  [current-direction current-char]
  (case current-char
    \W current-direction
    \# (get right current-direction)
    \F (get reversed current-direction)
    ;; \. or nil
    (get left current-direction)))

(defn next-state-2
  [{:keys [grid infected pos direction]}]
  (let [current (get grid pos)
        next-dir (next-direction-2 direction current)
        next-val (next-char-2 current)
        next-infected (if (infected? next-val)
                        (inc infected)
                        infected)
        next-grid (assoc grid pos next-val)
        next-pos (next-position pos next-dir)]
    {:grid next-grid
     :infected next-infected
     :pos next-pos
     :direction next-dir}))

(defn part-2
  []
  (:infected (nth (iterate next-state-2 (start)) 10000000)))

;;;; Scratch

(comment
  (set! *print-length* 20)
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  (time (part-1)) ;; ~10ms, 5259
  (time (part-2)) ;; ~10s,  2511722
  )
