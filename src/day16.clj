(ns day16
  (:require
   [blancas.kern.core :as k]
   [clojure.edn :as edn]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [criterium.core :refer [quick-bench]]
   [instaparse.core :as insta]
   [util :refer [read-first parse-int]]
   ))

(def programs (vec "abcdefghijklmnop"))

(defn data
  []
  (read-first "day16.txt"))

;;;; Instaparse

(def parse
  (insta/parser
   "<INPUT>       = INSTRUCTION (<','> INSTRUCTION)*
    <INSTRUCTION> = SPIN | EXCHANGE | PARTNER
    SPIN          = <'s'> POSITION
    EXCHANGE      = <'x'> POSITION <'/'> POSITION
    PARTNER       = <'p'> PROGRAM  <'/'> PROGRAM
    POSITION      = #'\\d\\d?'
    PROGRAM       = #'[a-p]'"))

(defn insta-parser
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
  ([] (part-1 programs (insta-parser (data))))
  ([progs parsed]
   (apply str (solve progs parsed))))

(defn part-2
  ([] (part-2 programs (insta-parser (data))))
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

(defn handwritten-parser
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

(defn kern-parser
  [d]
  (:value (k/parse-data parse-input d)))

;;;; clojure.spec parser

(s/def ::program
  #(and (>= (int %) (int \a))
        (<= (int %) (int \p))))

(comment
  (s/valid? ::program \a) ;; true
  (s/valid? ::program \z) ;; false
  )

(s/def ::position
  (s/&
   (s/+ #(Character/isDigit ^Character %))
   (s/conformer
    (fn [parsed]
      (Integer/parseInt
       (apply str parsed))))))

(comment
  (s/conform ::position (seq "123")) ;; 123
  (s/conform ::position "z") ;; :clojure.spec.alpha/invalid
  )

(s/def ::spin
  (s/and
   (s/cat :first #{\s}
          :pos ::position)
   (s/conformer (juxt :pos))))

(comment
  (s/conform ::spin (seq "s10")) ;; [10]
  )

(s/def ::partner
  (s/and
   (s/cat :first #{\p}
          :prog1 ::program
          :slash #{\/}
          :prog2 ::program)
   (s/conformer (juxt :prog1 :prog2))))

(comment
  (s/conform ::partner (seq "pa/g")) ;; [\a \g]
  )

(s/def ::exchange
  (s/and
   (s/cat :first #{\x}
          :pos1 ::position
          :slash #{\/}
          :pos2 ::position)
   (s/conformer (juxt :pos1 :pos2))))

(comment
  (s/conform ::exchange (seq "x11/2")) ;; [11 2]
  )

(s/def ::instruction
  (s/and
   (s/conformer seq)
   (s/or :SPIN ::spin
         :EXCHANGE ::exchange
         :PARTNER ::partner)
   (s/conformer (comp vec flatten))))

(comment
  (s/conform ::instruction "s10") ;; [:SPIN 10]
  (s/conform ::instruction "x11/2") ;; [:EXCHANGE 11 2]
  (s/conform ::instruction "pa/g") ;; [:PARTNER \a \g]
  )

(s/def ::instructions
  (s/and (s/conformer #(str/split % #","))
         (s/* ::instruction)))

(comment
  (s/conform ::instructions "s10,x11/2,pl/g") ;; [[:SPIN 10] [:EXCHANGE 11 2] [:PARTNER \l \g]]
  )

(defn spec-parser
  [input]
  (s/conform ::instructions input))

;;;; Scratch

(comment
  (set! *print-length* 20)
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  (quick-bench (doall (insta-parser (data))))       ;; Instaparse:   ~582ms
  (quick-bench (doall (handwritten-parser (data)))) ;; Handwritten:  ~7ms
  (quick-bench (doall (kern-parser (data))))        ;; Kern:         ~113ms
  (quick-bench (doall (spec-parser (data))))        ;; clojure.spec: ~413ms
  (= (insta-parser (data))
     (handwritten-parser (data))
     (kern-parser (data))
     (spec-parser (data))) ;; true
  (def parsed (handwritten-parser (data)))
  (quick-bench (part-1 programs parsed)) ;; ~7ms,   "olgejankfhbmpidc"
  (quick-bench (part-2 programs parsed)) ;; ~422ms, "gfabehpdojkcimnl"
  )
