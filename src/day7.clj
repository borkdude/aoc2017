(ns day7
  (:refer-clojure :exclude [ancestors])
  (:require
   [clojure.string :as str]
   [clojure.edn :as edn]
   [clojure.set :as set]
   [criterium.core :refer [quick-bench]]
   [util :refer [input resource-reducible]])
  (:import [java.lang Math]))

(set! *print-length* 20)

(defn data
  []
  (into []
        (comp (map #(format "[%s]" %))
              (map edn/read-string))
        (resource-reducible "day7.txt")))

(defn parse
  [[name [weight]
    & [arrow & names]]]
  (-> {}
      (assoc :node {:name name
                    :weight weight})
      (assoc :ancestors (into {}
                              (for [n names]
                                [n name])))))

(defn parse-all
  [data]
  (reduce (fn [acc l]
            (let [{:keys [node ancestors]}
                  (parse l)]
              (-> acc
                  (update :nodes assoc (:name node) node)
                  (update :ancestors merge ancestors))))
          {:nodes {}
           :ancestors {}}
          data))

(defn part-1
  []
  (let [{:keys [nodes ancestors]} (parse-all (data))
        has-ancestor (keys ancestors)
        names (into #{} (map :name (vals nodes)))
        without-ancestors (set/difference names
                                          has-ancestor)]
    (first without-ancestors)))

(defn tree-weight
  [parent child-map nodes]
  (let [children (get child-map parent)
        child-weights (map #(tree-weight % child-map nodes)
                           children)]
    (when (seq child-weights)
      (when-not (apply = child-weights)
        (throw (ex-info (format "Weights not equal %s %s"
                                parent
                                (pr-str child-weights))
                        (zipmap children
                                child-weights)))))
    (apply + (:weight (get nodes parent))
           child-weights)))

(defn unbalanced-balanced
  [vals]
  (->> (frequencies vals)
       (group-by val)
       sort
       (map #(second %))
       (map #(ffirst %))))

(defn find-by-val
  [m v]
  (ffirst
   (filter (fn [[k v']]
             (= v v'))
           m)))

(defn child-map
  [ancestors]
  (let [children (group-by val ancestors)]
    (zipmap (keys children)
            (map #(map first %)
                 (vals children)))))

(defn part-2
  []
  (let [{:keys [nodes ancestors]}
        (parse-all (data))
        cm (child-map ancestors)]
    (try (tree-weight (part-1) cm nodes)
         (catch Exception e
           (let [d (ex-data e)
                 [unbalanced balanced]
                 (unbalanced-balanced (vals d))
                 k (find-by-val d unbalanced)
                 diff (- balanced unbalanced)
                 w (:weight (get nodes k))]
             (+ w diff))))))

;;;; Scratch

(comment
  (part-1)
  (part-2))
