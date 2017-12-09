(ns day9-spec
  (:refer-clojure :exclude [ancestors])
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [criterium.core :refer [quick-bench]]
   [clojure.spec.alpha :as s]
   [util :refer [input resource-reducible]])
  (:import [java.lang Math]))

(set! *print-length* 20)

(def lcb \{)
(def rcb \})
(def lab \<)
(def rab \>)
(def exl \!)

(defn data
  []
  (input
   ;; "day9.txt"
   "day9-fellshard.txt"
   ;; "day9-bhauman.txt"
   ))

(defn rf [{:keys [level score gc
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
    :else state
    
    #_(throw (Exception. (str "unexpected" c)))))

(s/def ::garbage (s/cat :lab #{lab}
                        :content (s/* char?)
                        :rab #{rab}))

(s/conform ::garbage (seq "<<!x!>>"))

(s/def ::group (s/cat :lcb #{lcb}
                      :children
                      (s/* (s/cat :child (s/alt :group ::group
                                                :garbage ::garbage)
                                  :comma (s/? #{\,})))
                      :rcb #{rcb}))

(s/conform ::group (seq "{{},{<x>}}"))

(defn parse-string [s]
  (s/conform ::group (seq s)))

(comment (parse-string (data))
         (parse-string "{<x>,{<x>}}")
         (def p (parse-string (data))))

(defn solve [chars]
  (reduce rf {:level 1
              :score 0
              :gc 0
              :ignore? false
              :in-garbage? false
              :pos 0}
          chars))

(comment
  (solve "{<<<>}")
  )

#_(defn part-1
  []
  (-> (data) solve :score))

#_(defn part-2
  []
  (-> (data) solve :count))

;;;; Scratch

(comment
  (part-1) ;; 20530, fellshard: 17390,                bhauman: 13154
  (part-2) ;; 9978,  fellshard: 7825 (wrong so far),  bhauman: 6369 
  )
