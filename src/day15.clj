(ns day15
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [criterium.core :refer [quick-bench]]))

(def seed-a 591)
(def factor-a 16807)

(def seed-b 393)
(def factor-b 48271)

(def divider 2147483647)

(defn num-sequence
  [seed ^long factor]
  (rest
   (iterate (fn [^long v]
              (rem ^long (* v factor)
                   ^long divider))
            seed)))

(defn lowest-16-bits
  [^long n]
  (unchecked-short
   (bit-and n 0xffff)))

(defn count-same-bits
  [seed-a pred-a
   seed-b pred-b
   limit]
  (loop [seq-a (filter pred-a
                       (num-sequence
                        seed-a factor-a))
         seq-b (filter pred-b
                       (num-sequence
                        seed-b factor-b))
         n 0
         same-bits 0]
    (if (= n limit)
      same-bits
      (recur (rest seq-a)
             (rest seq-b)
             (inc n)
             (if (= (lowest-16-bits
                     (first seq-a))
                    (lowest-16-bits
                     (first seq-b)))
               (inc same-bits)
               same-bits)))))

(defn part-1
  []
  (count-same-bits seed-a identity
                   seed-b identity
                   40000000))

(defn multiple-of
  [^long x ^long y]
  (= (bit-or
      (bit-and
       (unchecked-byte x) 0xff)
      (- 256 y))
     (- 256 y)))

(defn part-2
  []
  (count-same-bits seed-a #(multiple-of % 4)
                   seed-b #(multiple-of % 8)
                   5000000))

;;;; Scratch

(comment
  (set! *print-length* 20)
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  (time (part-1)) ;; 619, ~11.5s
  (time (part-2)) ;; 290, ~7s
  )
