(ns day19
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [criterium.core :refer [quick-bench]]
   [util :refer [resource-reducible
                 spy take-until]]))

(defn data
  []
  (into []
        (resource-reducible "day19.txt")))

(defn columns
  [row]
  (into []
        (apply map str row)))

(defn substring
  "Substring of s including end"
  [s start end]
  (subs s start (inc ^long end)))

(defn explore
  [game x y]
  (when-let [row (get (:rows game) y)]
    (.charAt ^String row x)))

(def dxdy
  {:east  [1  0]
   :west  [-1 0]
   :north [0 -1]
   :south [0  1]})

(defn explore-direction
  [{:keys [^long x ^long y] :as game} direction]
  (let [[^long dx ^long dy] (get dxdy direction)
        x' (+ x dx)
        y' (+ y dy)]
    (explore game x' y')))

(defn update-direction
  [{:keys [direction] :as game}]
  (let [directions
        (if (#{:north :south}
             direction)
          [:east :west]
          [:north :south])]
    (let [next-direction
          (some
           (fn [dir]
             (let [e (explore-direction game dir)]
               (when (and e (not= \space e))
                 dir)))
           directions)]
      (assoc game :direction next-direction))))

(defn next-game
  ([game]
   (next-game game \space false))
  ([{:keys [^long x ^long y rows
            cols direction] :as game}
    c init?]
   (let [[x' y' s]
         (case direction
           :east
           (let [s  (get rows y)
                 x' (str/index-of s c x)
                 x' (if init? x' (dec ^long x'))
                 s' (substring s x x')]
             [x' y s'])
           :west
           (let [s  (get rows y)
                 s' (str/reverse (substring s 0 x))
                 dx (str/index-of s' c)
                 x' ^long (- x ^long dx)
                 x' (if init? x' (inc x'))
                 s' (substring s x' x)]
             [x' y s'])
           :south
           (let [s  (get cols x)
                 y' (str/index-of s c y)
                 y' (if init? y' (dec ^long y'))
                 s' (substring s y y')]
             [x y' s'])
           :north
           (let [s  (get cols x)
                 s' (str/reverse (substring s 0 y))
                 dy (str/index-of s' c)
                 y' (- y ^long dy)
                 y' (if init? y' (inc y'))
                 s' (substring s y' y)]
             [x y' s']))]
     (-> game
         (assoc :x x' :y y')
         (update :steps +
          (Math/abs (- x ^long x'))
          (Math/abs (- y ^long y')))
         (update :letters
                 #(apply str %
                         (re-seq #"[A-Z]" s)))
         (update-direction)))))

(defn solve
  [grid]
  (let [rows grid
        cols (columns rows)
        start (next-game
               {:x 0 :y 0
                :rows rows
                :cols cols
                :direction :east
                :steps 1
                :letters ""}
               \|
               true)
        game (assoc start :steps 1)]
    (take-until (comp not :direction)
                (iterate next-game game))))

(defn part-1
  []
  (apply str (:letters (last (solve (data))))))

(defn part-2
  []
  (:steps (last (solve (data)))))

;;;; Scratch

(comment
  (set! *print-length* 20)
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  (time (part-1)) ;; ~9ms, "GINOWKYXH"
  (time (part-2)) ;; ~9ms, 16636
  )
