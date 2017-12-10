(ns day04
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [net.cgrand.xforms :as x]
            [util :refer [resource-reducible]]))

(defn valid-passphrase?
  [phrase]
  (apply distinct? phrase))

(defn part-1
  []
  (x/count
   (comp
    (map #(str/split % #"\s"))
    (filter valid-passphrase?))
   (resource-reducible "day4.txt")))

;; first I checked for palindromes, but then realized they are not
;; anagrams, which set me on the wrong foot to refactor to this
;; inefficient solution. my excuse is that I just woke up and didn't
;; have coffee yet :-D

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
  (x/count
   (comp
    (map #(str/split % #"\s"))
    (filter valid-passphrase?)
    (filter valid-anagram-passphrase?))
   (resource-reducible "day4.txt")))

(defn part-2-sort
  "See https://github.com/krisajenkins/AdventOfCode/blob/master/src/Year2017/Day4.purs"
  []
  (x/count
   (comp
    (map #(str/split % #"\s"))
    (map #(map sort %))
    (filter valid-passphrase?))
   (resource-reducible "day4.txt")))

(defn part-2-frequencies
  []
  (x/count
   (comp
    (map #(str/split % #"\s"))
    (map frequencies)
    (filter valid-passphrase?))
   (resource-reducible "day4.txt")))

(def part-2 part-2-frequencies)

;;;; Scratch

(comment
  (time (part-1))
  (time (part-2-naive))        ;; slow, about 2.5 s
  (time (part-2-sort))         ;; much better, 5 ms
  (time (part-2-frequencies))  ;; about the same as sort

  (valid-passphrase? ["foo" "bar"])
  (valid-passphrase? ["foo" "foo"])
  )
