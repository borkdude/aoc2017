(ns day21
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [criterium.core :refer [quick-bench]]
   [util :refer [resource-reducible]]
   [clojure.set :as set]
   [flatland.ordered.map :refer [ordered-map]]))

(def start
  ".#./..#/###")

(defn rotation-2
  [^String input]
  (let [out (StringBuilder. input)]
    (doseq [[k v] {0 3
                   1 0
                   3 4
                   4 1}]
      (.setCharAt out v (get input k)))
    (str out)))

(defn rotations-2
  [^String input]
  (take 4 (iterate rotation-2 input)))

(defn rotation-3
  [^String input]
  (let [out (StringBuilder. input)]
    (doseq [[k v] {0 8
                   1 4
                   2 0
                   4 9
                   6 1
                   8 10
                   9 6
                   10 2}]
      (.setCharAt out v (get input k)))
    (str out)))

(defn rotations-3
  [^String input]
  (take 4 (iterate rotation-3 input)))

(defn flip-horizontally-3
  [^String input]
  (let [out (StringBuilder. input)]
    (doseq [[k v] {0 2
                   2 0
                   6 4
                   4 6
                   8 10
                   10 8}]
      (.setCharAt out v (get input k)))
    (str out)))

(defn rules
  [file]
  (transduce
   (comp
    (map #(str/split % #" => "))
    (mapcat (fn [[k v]]
              (if (= 5 (count k))
                (map #(vector % v)
                     (rotations-2 k))
                (map #(vector % v)
                     (concat
                      (rotations-3 k)
                      (rotations-3
                       (flip-horizontally-3 k))))))))
   (fn
     ([acc] acc)
     ([acc [k v]]
      ;; Don't override existing rules. Order may matter.
      (if (get acc k)
        acc
        (assoc acc k v))))
   (ordered-map)
   (resource-reducible file)))

(defn dissect-grid
  [grid]
  (let [grid (str/split grid #"/")
        _ (assert (= (count grid)
                     (count (first grid)))
                  "Not a grid")
        square (if (zero?
                    ^long (mod (count grid) 2))
                 2 3)]
    (->>
     grid
     (partition square)
     (mapv (fn [strings]
             (mapv #(partition square %)
                   strings)))
     (mapv #(apply map vector %))
     (mapv #(mapv (fn [g] (interpose \/ g)) %))
     (mapv #(mapv (fn [g] (apply str (flatten g))) %)))))

(defn build-grid
  [lines-of-groups]
  (->> lines-of-groups
       (mapv (fn [line]
               (mapv (fn [group]
                       (str/split group #"/"))
                     line)))
       (mapv #(apply mapv vector %))
       (mapcat (fn [line]
                 (mapv #(apply str %)
                       line)))
       (str/join "/")))

(defn pixels-on
  [grid]
  (count (filter #(= \# %) grid)))

(defn solve1
  [rules start]
  (iterate
   (comp
    build-grid
    (fn [lines]
      (mapv
       (fn [line]
         (mapv
          (fn [square]
            (get rules square))
          line))
       lines))
    dissect-grid)
   start))

(defn part-1
  []
  (pixels-on
   (nth (solve1
         (rules "day21.txt")
         start)
        5)))

(defn part-2
  []
  (pixels-on
   (nth
    (solve1 (rules "day21.txt")
            start)
    18)))

;;;; Scratch

(comment
  (set! *print-length* 20)
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  (time (part-1)) ;; 197, 3ms
  (time (part-2)) ;; 3081737, ~8.5s
  )
