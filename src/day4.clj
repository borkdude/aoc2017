(ns day4
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [util :refer [resource-reducible]]))

(defn valid-passphrase?
  [phrase]
  (distinct? phrase))

(defn part-1
  []
  (transduce
   (comp
    (map #(str/split % #"\s"))
    (filter valid-passphrase?)
    (map (constantly 1)))
   +
   (resource-reducible "day4.txt")))

(defn anagrams
  [word]
  (map #(apply str %)
       (rest (combo/permutations word))))

(defn anagram-in-words?
  [words word]
  (some #(contains? words
                    %)
        (anagrams word)))

(defn valid-anagram-passphrase?
  [words]
  (let [words (set words)]
    (not (some #(anagram-in-words?
                 (disj words %) %)
               words))))

(defn part-2-naive
  []
  (transduce
   (comp
    (map #(str/split % #"\s"))
    (filter valid-passphrase?)
    (filter valid-anagram-passphrase?)
    (map (constantly 1)))
   +
   (resource-reducible "day4.txt")))

(defn part-2-sort
  "See https://github.com/krisajenkins/AdventOfCode/blob/master/src/Year2017/Day4.purs"
  []
  (transduce
   (comp
    (map #(str/split % #"\s"))
    (map #(map sort %))
    (filter valid-passphrase?)
    (map (constantly 1)))
   +
   (resource-reducible "day4.txt")))

(defn part-2-frequencies
  []
  (transduce
   (comp
    (map #(str/split % #"\s"))
    (map frequencies)
    (filter valid-passphrase?)
    (map (constantly 1)))
   +
   (resource-reducible "day4.txt")))

;;;; Scratch

(comment
  (time (part-1))
  (time (part-2-naive))        ;; slow, about 2.5 s
  (time (part-2-sort))         ;; much better, 2 ms
  (time (part-2-frequencies))  ;; better 11 ms, but sort is faster
  )
