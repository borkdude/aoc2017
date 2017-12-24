(ns day24
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [criterium.core :refer [quick-bench]]
   [util :refer [resource-reducible]]))

(defn data
  []
  (eduction
   (comp
    (map #(str/replace % #"/" " "))
    (map #(format "[%s]" %))
    (map edn/read-string)
    (map set))
   (resource-reducible "day24.txt")))

(defn find-connecting
  [n components]
  (filter #(contains? % n)
          components))

(defn solutions
  ([components]
   (solutions components 0))
  ([components n]
   (when-let [connecting
              (seq (find-connecting n components))]
     (for [c connecting]
       (let [other (or
                    (first
                     (disj c n))
                    n)]
         (cons c
               (solutions
                (disj components c)
                other)))))))

(defn component-strength ^long
  [component]
  (let [[^long a ^long b] (seq component)]
    (if (and a b)
      (+ a b)
      (+ a a))))

(defn solution-strengths
  ([solution]
   (solution-strengths 0 solution))
  ([^long strength [head & rst]]
   (let [strength-head (component-strength head)
         strength' (+ strength-head strength)]
     (if (seq rst)
       (map #(solution-strengths strength' %) rst)
       strength'))))

(defn part-1
  []
  (let [sols (solutions (into #{} (data)))
        strengths (map solution-strengths sols)]
    (apply max (flatten strengths))))

(defn lengths-and-strengths
  ([solution]
   (lengths-and-strengths [0 0] solution))
  ([[^long length ^long strength] [head & rst]]
   (let [strength-head (component-strength head)
         strength' (+ strength-head strength)
         new-lw [(inc length) strength']]
     (if (seq rst)
       (map #(lengths-and-strengths
              new-lw %) rst)
       new-lw))))

(defn flatten*
  [x]
  (filter vector?
          (rest (tree-seq
                 (comp not vector?)
                 seq x))))

(defn part-2
  []
  (let [sols (solutions (into #{}(data)))
        lws (map lengths-and-strengths sols)
        lws-sorted (sort (comp - compare)
                         (flatten* lws))]
    (second (first lws-sorted))))

;;;; Refactor

(defn component-index
  [eduction]
  (reduce
   (fn [acc c]
     (let [[a b] (seq c)]
       (cond->
           (update acc a (fnil conj #{}) c)
         b (update b (fnil conj #{}) c))))
   {}
   eduction))

(defn remove-component
  [index c]
  (let [[a b] (seq c)]
    (cond-> (update index a disj c)
      b (update b disj c))))

(defn solutions'
  ([idx]
   (solutions' idx 0 0 0))
  ([idx n ^long length ^long strength]
   (if-let [connecting
            (seq (get idx n))]
     (mapcat
      (fn [c]
        (let [strength' (+ strength
                           (component-strength c))
              length' (inc length)
              other (or
                     (first
                      (disj c n))
                     n)]
          (solutions'
           (remove-component idx c)
           other
           length'
           strength')))
      connecting)
     [[length strength]])))

(defn part-1'
  []
  (let [sols (solutions' (component-index (data)))]
    (second (apply max-key second sols))))

(defn part-2'
  []
  (let [sols (solutions' (component-index (data)))]
    (second
     (first
      (sort (comp - compare) sols)))))

;;;; Scratch

(comment
  (set! *print-length* 20)
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  (quick-bench (part-1)) ;;  1859, ~5.65s
  (quick-bench (part-1')) ;; 1859, ~1.60s
  (quick-bench (part-2)) ;;  1799, ~5.97s
  (quick-bench (part-2')) ;; 1799, ~1.85s
  )
