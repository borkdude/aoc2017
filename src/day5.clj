(ns day5
  (:require [util :refer [resource-reducible]]))

(defn part-1 []
  (loop [maze (into []
                    (map #(Integer/parseInt %))
                    (resource-reducible "day5.txt"))
         cur-pos 0
         steps 0]
    (if-let [cur-val (get maze cur-pos)]
      (let [next-maze (assoc maze cur-pos (inc cur-val))
            next-pos (+ cur-pos cur-val)]
        (recur next-maze next-pos (inc steps)))
      steps)))

(defn part-2 []
  (loop [maze
         (into []
               (map #(Integer/parseInt %))
               (resource-reducible "day5.txt"))
         cur-pos 0
         steps 0]
    (if-let [cur-val (get maze cur-pos)]
      (let [next-maze
            (assoc maze cur-pos
                   (if (>= cur-val 3)
                     (dec cur-val)
                     (inc cur-val)))
            next-pos (+ cur-pos cur-val)]
        (recur next-maze next-pos (inc steps)))
      steps)))

(defn part-2-faster []
  (loop [maze
         (transient
          (into []
                (map #(Integer/parseInt %))
                (resource-reducible "day5.txt")))
         cur-pos 0
         steps 0]
    (if-let [cur-val (get maze cur-pos)]
      (let [next-maze
            (assoc! maze cur-pos
                    (if (>= cur-val 3)
                      (dec cur-val)
                      (inc cur-val)))
            next-pos (+ cur-pos cur-val)]
        (recur next-maze next-pos (inc steps)))
      steps)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn part-2-array []
  (let [^ints maze
        (->>
         (into []
               (map #(Integer/parseInt %))
               (resource-reducible "day5.txt"))
         (into-array Integer/TYPE))
        length ^int (alength maze)]
    (loop [maze maze
           cur-pos 0
           steps 0]
      (if-let [^int cur-val
               (and (< cur-pos
                       length)
                    (aget maze cur-pos))]
        (recur (doto maze
                 (aset
                  cur-pos
                  (if (>= cur-val 3)
                    (dec cur-val)
                    (inc cur-val))))
               (+ cur-pos cur-val)
               (inc steps))
        steps))))

(comment
  (time (part-1))
  (time (part-2)) ;; 5.6 s, not fast enough for me
  (time (part-2-faster)) ;; 3.8 s, still sucks
  (time (part-2-array)) ;; 240 ms
  )
