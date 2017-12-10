(ns day09-instaparse
  "Solution for day 9 using Instaparse."
  (:require
   [util :refer [read-first]]
   [instaparse.core :as insta]))

(defn data
  []
  (read-first "day9.txt"))

(def parse
  (insta/parser
   "GROUP      = <'{'> CHILDREN <'}'> 
    <CHILDREN> = CHILD*
    <CHILD>    = (GROUP | GARBAGE) <','?>
    GARBAGE    = < '<' > ( NORMAL | ESCAPED)* < '>' >
    <ESCAPED>  = < #'(!.)+' >
    <NORMAL>   = #'[^!>]+'"))

(defn tree-weight
  [depth [node & leafs]]
  (case node
    :GROUP
    (apply + depth
           (map #(tree-weight
                  (inc depth )%)
                leafs))
    :GARBAGE 0))

(defn garbage-count
  [[node & leafs]]
  (case node
    :GARBAGE
    (apply + (map count
                  leafs))
    :GROUP
    (apply + (map garbage-count
                  leafs))))

(defn part-1
  []
  (tree-weight 1 (parse (data))))

(defn part-2
  []
  (garbage-count (parse (data))))

;;;; Scratch

(comment
  (part-1) ;; 20530, fellshard: 17390, bhauman: 13154
  (part-2) ;; 9978,  fellshard: 7825,  bhauman: 6369
  (time (parse (data))) ;; 380 ms
  )
