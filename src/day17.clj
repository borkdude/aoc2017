(ns day17)

(defn insert [vs pos v]
  (-> 
   (into (subvec vs 0 pos)
         [v])
   (into (subvec vs pos))))

(defn solve
  [^long step-size ^long n]
  (reduce
   (fn [[buffer ^long pos] ^long v]
     (let [pos' (inc ^long
                     (mod (+ pos step-size) v))]
       [(insert buffer pos' v)
        pos']))
   [[0] 0]
   (rest (range (inc n)))))

(defn solve2
  ;; here we only have to keep track of the second position
  [^long step-size ^long n]
  (reduce
   (fn [[pos-1 ^long pos] ^long v]
     (let [pos' (inc ^long (mod (+ pos step-size) v))]
       [(if (= 1 pos')
          v
          pos-1)
        pos']))
   [nil 0]
   (rest (range (inc n)))))

(defn part-1
  ([]
   (let [buffer (first (solve 354 2017))
         i (.indexOf ^clojure.lang.PersistentVector
                     buffer 2017)]
     (get buffer (inc i)))))

(defn part-2
  ([]
   (first (solve2 354 50000000))))

;;;; Scratch

(comment
  (set! *print-length* 20)
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  (time (part-1)) ;; ~100 ms, 2000 
  (time (part-2)) ;; ~2s, 10242889  
  )
