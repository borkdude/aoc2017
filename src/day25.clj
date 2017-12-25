(ns day25
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]))

(def transitions
  '{[A 0] [1  1  B]
    [A 1] [0 -1  E]
    [B 0] [1 -1  C]
    [B 1] [0  1  A]
    [C 0] [1 -1  D]
    [C 1] [0  1  C]
    [D 0] [1 -1  E]
    [D 1] [0 -1  F]
    [E 0] [1 -1  A]
    [E 1] [1 -1  C]
    [F 0] [1 -1  E]
    [F 1] [1  1  A]})

(defn next-game
  [{:keys [state tape ^long position]}]
  (let [cur-val (get tape position 0)
        [to-write ^long dir next-state]
        (get transitions [state cur-val])]
    {:state next-state
     :tape (assoc tape position to-write)
     :position (+ position dir)}))

(defn solve
  []
  (nth (iterate
        next-game
        {:state 'A
         :tape {}
         :position 0})
       12208951))

(defn part-1
  []
  (apply + (vals (:tape (solve)))))

(defn part-2
  []
  "Rebooted the printer!")

;;;; Scratch

(comment
  (set! *print-length* 20)
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  (time (part-1)) ;; 4387, ~10s 
  (time (part-2)) ;; ~0.03ms 
  )
