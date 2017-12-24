(ns day24
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [util :refer [resource-reducible]]))

(defn data
  []
  (into #{}
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
  (let [sols (solutions (data))
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
  (let [sols (solutions (data))
        lws (map lengths-and-strengths sols)
        lws-sorted (sort (comp - compare)
                         (flatten* lws))]
    (second (first lws-sorted))))

;;;; Refactor

(defn solutions'
  ([components]
   (solutions' components 0 0 0))
  ([components n length strength]
   (if-let [connecting
            (seq (find-connecting n components))]
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
           (disj components c)
           other
           length'
           strength')))
      connecting)
     [[length strength]])))

(defn part-1'
  []
  (let [sols (solutions' (data))]
    (second (apply max-key second sols))))

(defn part-2'
  []
  (let [sols (solutions' (data))]
    (second
     (first
      (sort (comp - compare) sols)))))

;;;; Scratch

(comment
  (set! *print-length* 20)
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  (time (part-1)) ;; 1859, ~5.8s
  (time (part-1')) ;; 1859 ~4.8ms
  (time (part-2)) ;; 1799, ~6.4s
  (time (part-2')) ;; 1799, ~5.0s
  )
