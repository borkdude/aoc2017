(ns day9-spec
  "Solution for day 9 with Spec. Not finished."
  (:refer-clojure :exclude [ancestors])
  (:require
   [clojure.edn :as edn]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [criterium.core :refer [quick-bench]]
   [util :refer [input resource-reducible]])
  (:import [java.lang Math]))

(set! *print-length* 20)

(def lcb \{)
(def rcb \})
(def lab \<)
(def rab \>)
(def exl \!)
(def comma \,)

(defn data
  []
  (input
   ;; "day9.txt"
   "day9-fellshard.txt"
   ;; "day9-bhauman.txt"
   ))

(s/def ::escaped (s/cat :exl #{exl} :char char?))

(s/def ::garbage (s/cat :lab #{lab}
                        :content
                        (s/* (s/alt :escaped ::escaped
                                    :char    char?))
                        :rab #{rab}))

(s/def ::group (s/cat :lcb #{lcb}
                      :children
                      (s/* (s/cat
                            :child (s/alt :group ::group
                                          :garbage ::garbage)
                            :comma (s/? #{comma})))
                      :rcb #{rcb}))

(defn parse
  [s]
  (s/conform ::group (seq s)))

(comment
  (parse (str (apply str lcb (repeat 1000 "{{},{},{},{},{{{{{{<!>}}}}}}}"))
              rcb))
  (parse "{{<<!!!x>}}")
  (def p (parse (data))) ;; why so slow?
  )

(defn part-1
  []
  )

(defn part-2
  []
  )

;;;; Scratch

(comment
  (part-1) ;; 20530, fellshard: 17390,                bhauman: 13154
  (part-2) ;; 9978,  fellshard: 7825 (wrong so far),  bhauman: 6369 
  )
