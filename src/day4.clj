(ns day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [util :refer [resource-reducible]]))

(defn valid-passphrase? [phrase]
  (= (count phrase)
     (count (set phrase))))

(defn part-1 []
  (transduce
   (comp
    (map #(str/split % #"\s"))
    (keep #(when (valid-passphrase? %)
             1)))
   +
   (resource-reducible "day4.txt")))

(defn anagrams [word]
  (map #(apply str %)
       (combo/permutations word)))

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

(defn part-2 []
  (transduce
   (comp
    (map #(str/split % #"\s"))
    (filter #(valid-passphrase? %))
    (keep #(when (valid-anagram-passphrase? %)
             1)))
   +
   (resource-reducible "day4.txt")))

;;;; Scratch

(comment
  (part-1)
  (part-2)
  )
