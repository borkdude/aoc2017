(ns day8
  (:refer-clojure :exclude [ancestors])
  (:require
   [clojure.edn :as edn]
   [clojure.set :as set]
   [clojure.string :as str]
   [criterium.core :refer [quick-bench]]
   [util :refer [input resource-reducible]])
  (:import [java.lang Math]))

(set! *print-length* 20)

(defn data
  []
  (into []
        (comp (map #(format "[%s]" %))
              (map edn/read-string))
        (resource-reducible "day8.txt")))

(defn lookup-operation
  [op]
  (or (get {'inc +
            'dec -
            '!=  not=}
           op
           (resolve op))
      (throw (Exception. (str "cannot resolve " op)))))

;; By accident, the max returned by solve was the answer for part 2,
;; so I didn't have to change the code for that part. The error for
;; part 1 was that the register that already had the maximum could
;; change to a lower value, then cur-max was not the valid maximum
;; anymore.
(defn solve
  ""
  [data]
  (let [end-state
        (reduce
         (fn [[cpu cur-max]
              [r1 op1 v1
               _
               r2 op2 v2]]
           (let [old (get cpu r1 0)
                 new ((lookup-operation op1)
                      old
                      v1)
                 yes? ((lookup-operation op2)
                       (get cpu r2 0)
                       v2)
                 new-cpu (if yes?
                           (assoc cpu r1 new)
                           cpu)
                 new-max (if yes?
                           (max cur-max new)
                           cur-max)]
             [new-cpu new-max]))
         [{} 0]
         data)]
    end-state))

(defn part-1
  []
  (apply max (vals (first (solve (data))))))

(defn part-2
  []
  (second (solve (data))))

;;;; Scratch

(comment
  (quick-bench (part-1)) ;; 5221, 5.6 ms
  (quick-bench (part-2)) ;; 7491, 5.6 ms


  )
