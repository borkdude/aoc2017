(ns day3
  (:import [java.lang Math]))

(def directions
  (let [dirs (cycle [[:right :up] [:left :down]])
        amount (map inc (range))]
    (mapcat (fn [[d1 d2] amount]
              (concat (repeat amount d1)
                      (repeat amount d2)))
            dirs
            amount)))

(defn update-tile [tile direction]
  (->
   (case direction
     :right
     (update tile
             :x inc)
     :left
     (update tile
             :x dec)
     :down
     (update tile
             :y inc)
     :up
     (update tile
             :y dec))
   (update :n inc)))

(defn tile-at [n]
  (reduce
   (fn [tile next-direction]
     (if (= (:n tile) n)
       (reduced tile)
       (update-tile tile next-direction)))
   {:x 0 :y 0 :n 1}
   directions))

(defn part-1 []
  (let [tile (tile-at 289326)]
    (+ (Math/abs (:x tile))
       (Math/abs (:y tile)))))

(defn sum-of-neighbours [{:keys [x y]}
                         tiles]
  (let [neighbour-positions [[(dec x) y]
                             [(inc x) y]
                             [x (dec y)]
                             [x (inc y)]
                             [(dec x) (dec y)]
                             [(dec x) (inc y)]
                             [(inc x) (dec y)]
                             [(inc x) (inc y)]]
        neighbours (keep #(get tiles %)
                         neighbour-positions)]
    (reduce +' (map :v neighbours))))

(defn tile-with-bigger-sum [n]
  (let [init-tile {:x 0 :y 0 :n 1 :v 1}]
    (loop [tile init-tile
           tiles {[0 0] init-tile}
           directions directions]
      (let [next-direction (first directions)]
        (let [new-tile (update-tile tile next-direction)
              new-tile (assoc new-tile :v (sum-of-neighbours
                                           new-tile
                                           tiles))]
          (if (> (:v new-tile)
                 n)
            new-tile
            (recur new-tile
                   (assoc tiles [(:x new-tile)
                                 (:y new-tile)]
                          new-tile)
                   (rest directions))))))))

(defn part-2 []
  (:v (tile-with-bigger-sum 289326)))
