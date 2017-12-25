(ns day16
  (:require
   [blancas.kern.core :as k]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [criterium.core :refer [quick-bench]]
   [instaparse.core :as insta]
   [util :refer [read-first parse-int]]
   ))

(def programs (vec "abcdefghijklmnop"))

(defn data
  []
  (read-first "day16.txt"))

(def parse
  (insta/parser
   "<INPUT>       = INSTRUCTION (<','> INSTRUCTION)*
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

(def dispatch
  {:SPIN spin
   :EXCHANGE exchange
   :PARTNER partner})

(defn eval-expr
  [progs [tag & args]]
  (let [f (dispatch tag)]
    (apply f 
     progs
     args)))

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

;;;; For comparison, a handwritten parser

(defn but-first-char
  [s]
  (subs s 1 (count s)))

(defn parse-args
  [s]
  (str/split s #"/"))

(defn parse-data2
  [d]
  (map
   (fn [expr]
     (case (first expr)
       \s [:SPIN (parse-int
                  (but-first-char expr))]
       \x (into [:EXCHANGE]
                (mapv parse-int
                      (parse-args
                       (but-first-char expr))))
       \p (into [:PARTNER]
                (mapv
                 first
                 (parse-args
                  (but-first-char expr))))))
   (str/split d #",")))

;;;; Kern parser

(def parse-partner
  (k/bind [_  (k/sym* \p)
           p1 k/letter
           _ (k/sym* \/)
           p2 k/letter]
          (k/return [:PARTNER p1 p2])))

(def parse-exchange
  (k/bind [_ (k/sym* \x)
           pos1 k/dec-num
           _ (k/sym* \/)
           pos2 k/dec-num]
          (k/return [:EXCHANGE pos1 pos2])))

(def parse-spin
  (k/bind [_ (k/sym* \s) n k/dec-num]
          (k/return [:SPIN n])))

(def parse-instruction
  (k/<|> parse-spin parse-exchange parse-partner))

(def parse-input
  (k/sep-by1 (k/sym* \,) parse-instruction))

(defn parse-data3
  [d]
  (:value (k/parse-data parse-input d)))

;;;; Scratch

(comment
  (set! *print-length* 20)
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  (quick-bench (def parsed1 (doall (parse-data (data)))))  ;; ~582ms
  (quick-bench (def parsed2 (doall (parse-data2 (data))))) ;; ~7.8ms
  (quick-bench (def parsed3 (parse-data3 (data))))         ;; ~113ms
  (= parsed1 parsed2 parsed3) ;; true
  (quick-bench (part-1 programs parsed1)) ;; ~7ms,   "olgejankfhbmpidc"
  (quick-bench (part-2 programs parsed1)) ;; ~422ms, "gfabehpdojkcimnl"
  )
