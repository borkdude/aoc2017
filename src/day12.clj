(ns day12
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [util :refer [resource-reducible]]))

(defn data
  []
  (into {}
        (comp
         (map #(format "[%s]" %1))
         (map edn/read-string)
         (map (fn [[from _arrow & tos]]
                [from (disj (set tos) from)])))
        (resource-reducible "day12.txt")))

(defn with-indirect-neighbours
  "Returns neighbour-map with indirect neighbours added to key k"
  [k neighbour-map]
  (let [neighbours (get neighbour-map k)
        indirect-neighbours
        (reduce into neighbours
                (for [n neighbours]
                  (get neighbour-map n)))]
    (assoc neighbour-map k indirect-neighbours)))

(defn transitive-neighbours
  "Returns neighbour-map with neighbours expanded to transitive
  neighbours for key k"
  [k neighbour-map]
  (reduce (fn [prev-state next-state]
            (if (= (get prev-state k)
                   (get next-state k))
              (reduced (get prev-state k))
              next-state))
          (iterate (partial with-indirect-neighbours k)
                   neighbour-map)))

(defn part-1
  []
  (count (transitive-neighbours 0 (data))))

(defn part-2
  []
  (count
   (distinct
    (map #(transitive-neighbours % (data))
         (keys (data))))))

(defn with-indirect-neighbours-all
  "Same as with-indirect-neighbours but for all keys."
  [neighbour-map]
  (reduce (fn [acc k]
            (let [neighbours (get acc k)
                  indirect-neighbours
                  (reduce into neighbours
                          (for [n neighbours]
                            (get acc n)))]
              (assoc acc k indirect-neighbours)))
          neighbour-map
          (keys neighbour-map)))

(defn transitive-neighbours-all
  "Same as transitive-neighbours but for all keys."
  [input]
  (reduce (fn [prev-state next-state]
            (if (= prev-state
                   next-state)
              (reduced prev-state)
              next-state))
          (iterate with-indirect-neighbours-all input)))

(defn part-2-faster
  []
  (count
   (distinct
    (vals
     (transitive-neighbours-all (data))))))

;;;; Borrowed from https://github.com/bhauman/advent-of-clojure-2016/blob/master/src/advent_of_clojure_2017/day12.clj#L23.
;;;; All glory to Bruce.

(defn bhauman-group
  [idx root]
  (into #{}
        (tree-seq (let [seen (atom #{})]
                    (fn [x] (when-not (@seen x)
                              (swap! seen conj x))))
                  idx
                  root)))

;;;; Scratch

(comment
  (set! *print-length* 20)
  (time (part-1)) ;; 288, 70ms
  (time (part-2)) ;; 211, 16.8s
  (time (part-2-faster)) ;; 211, 6.6s
  )
