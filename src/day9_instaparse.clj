(ns day9-spec
  "Solution for day 9 with Spec. Not finished."
  (:refer-clojure :exclude [ancestors])
  (:require
   [clojure.edn :as edn]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [criterium.core :refer [quick-bench]]
   [util :refer [input resource-reducible]]
   [instaparse.core :as insta])
  (:import [java.lang Math]))

(set! *print-length* 20)

(defn data
  []
  (input
   ;; "day9.txt"
   "day9-fellshard.txt"
   ;; "day9-bhauman.txt"
   ))

(def parse
  (insta/parser
   (input "day9.grammar")))

(defn leaf-weight [depth [tag & children]]
  (case tag
    :GROUP
    (apply + depth
           (map #(leaf-weight (inc depth)
                              %)
                children))
    :GARBAGE 0))

(defn garbage-count [[tag & children]]
  (case tag
    :GARBAGE
    (apply + (map count
                  children))
    :GROUP
    (apply + (map garbage-count
                  children))))

(defn part-1
  []
  (leaf-weight 1 (parse (data))))

(defn part-2
  []
  (garbage-count (parse (data))))

;;;; Scratch

(comment
  (part-1) ;; 20530, fellshard: 17390, bhauman: 13154
  (part-2) ;; 9978,  fellshard: 7825,  bhauman: 6369
  )
