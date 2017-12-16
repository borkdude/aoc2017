(ns day16
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [instaparse.core :as insta]
   [criterium.core :refer [quick-bench]]
   [util :refer [read-first parse-int]]))

(def programs (vec "abcdefghijklmnop"))

(defn data
  []
  (read-first "day16.txt"))

(def parse
  (insta/parser
   "<INPUT>       = (INSTRUCTION <','>)+ INSTRUCTION 
    <INSTRUCTION> = SPIN | EXCHANGE | PARTNER
    SPIN          = <'s'> POSITION
    EXCHANGE      = <'x'> POSITION <'/'> POSITION
    PARTNER       = <'p'> PROGRAM  <'/'> PROGRAM
    POSITION      = #'\\d\\d?'
    PROGRAM       = #'[a-p]'"))

(defn parse-data
  [d]
  (->>
   d
   parse
   (insta/transform
    {:POSITION parse-int
     :PROGRAM  first})))

(defn spin
  [progs ^long n]
  (let [c (count progs)
        r (- c n)]
    (into (subvec progs r c)
          (subvec progs 0 r))))

(defn exchange
  [progs x y]
  (-> progs
      (assoc x (progs y))
      (assoc y (progs x))))

(defn partner
  [^clojure.lang.PersistentVector progs
   x y]
  (exchange progs
            (.indexOf progs x)
            (.indexOf progs y)))

(defn eval-expr
  [progs [tag & args]]
  (apply ({:SPIN spin
           :EXCHANGE exchange
           :PARTNER partner} tag)
         progs
         args))

(defn solve
  [progs parsed]
  (reduce eval-expr
          progs
          parsed))

(defn part-1
  ([] (part-1 programs (parse-data (data))))
  ([progs parsed]
   (apply str (solve progs parsed))))

(defn part-2
  ([] (part-2 programs (parse-data (data))))
  ([progs parsed]
   (let [iters (iterate #(solve % parsed) progs)
         n-repeat (inc
                   (count
                    (take-while #(not= progs %)
                                (rest iters))))
         remaining (rem 1000000000 n-repeat)]
     (apply str (nth iters remaining)))))

;;;; Scratch

(comment
  (set! *print-length* 20)
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  (quick-bench (def parsed (parse-data (data)))) ;; ~842ms
  (quick-bench (part-1 programs parsed))         ;; ~7ms,   "olgejankfhbmpidc"
  (quick-bench (part-2 programs parsed))         ;; ~422ms, "gfabehpdojkcimnl"
  )
