(ns day20
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [util :refer [resource-reducible]]))

(defn data
  []
  (into []
        (comp
         (map #(str/replace % #"[<>=pva]" ""))
         (map #(format "[%s]" %))
         (map edn/read-string))
        (resource-reducible "day20.txt")))

(defn update-particle
  [[^long px ^long py ^long pz ^long
    vx ^long vy ^long vz ^long
    ax ^long ay ^long az]]
  (let [vx (+ vx ax)
        vy (+ vy ay)
        vz (+ vz az)]
    [(+ px vx) (+ py vy) (+ pz vz)
     vx        vy        vz
     ax ay az]))

(defn manhattan
  [[^long px ^long py ^long pz & _ :as particle]]
  (when particle
    (+ (Math/abs px)
       (Math/abs py)
       (Math/abs pz))))

(defn closest
  [particles]
  (apply min-key manhattan particles))

(defn solve1
  [particles]
  (mapv update-particle particles))

(defn converge
  ([iterations comp ^long init ^long max]
   (converge iterations comp
             (first (drop init iterations))
             (* 2 init) max))
  ([iterations comp prev n max]
   (let [iterations (drop n iterations)
         new (first iterations)
         cprev (comp prev)
         cnew  (comp new)
         next-n (* 2 ^long n)]
     (if (= cprev cnew)
       cprev
       (if (> next-n ^long max)
         (throw (ex-info "Not converged below max." {}))
         (recur iterations comp new next-n max))))))

(defn part-1
  []
  (converge (iterate solve1 (data))
            #(.indexOf ^clojure.lang.PersistentVector %
                       (closest %))
            512
            1000000))

(defn position
  [[^long px ^long py ^long pz & _ :as particle]]
  (when particle [px py pz]))

(defn remove-by-position
  [particles [x y z]]
  (set
   (remove (fn [[px py pz & _]]
             (= [x y z]
                [px py pz]))
           particles)))

(defn remove-colliding
  [particles]
  (first
   (reduce
    (fn [[removed positions]
         particle]
      (let [pos (position particle)]
        (if (contains? positions pos)
          [(remove-by-position removed pos)
           positions]
          [(conj removed particle)
           (conj positions pos)])))
    [#{} #{}]
    particles)))

(defn solve2
  [particles]
  (remove-colliding
   (into #{} (map update-particle particles))))

(defn part-2
  []
  (converge (iterate solve2 (set (data)))
            count
            512
            100000))

;;;; Scratch

(comment
  (set! *print-length* 20)
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  (time (part-1)) ;; 157, ~116ms 
  (time (part-2)) ;; 499, ~1.2s
  )
