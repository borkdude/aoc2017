(ns template
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [criterium.core :refer [quick-bench]]
   [util :refer [resource-reducible
                 parse-int find-first]]))

(defn data
  []
  (into []
        (comp
         (map #(format "[%s]" %))
         (map edn/read-string))
        (resource-reducible "day18.txt")))

(comment
  (next '(1 2 3)))

(defn get-val
  [registers reg]
  (if (symbol? reg)
    (get registers reg 0) reg))

(defn solve1
  [instructions]
  (loop [ctr 0
         registers {}
         last-played nil]
    (let [[instr reg arg] (get instructions ctr)]
      #_(println ctr instr reg arg registers)
      (case instr
        set (recur (inc ctr)
                   (assoc registers reg
                          (get-val registers arg))
                   last-played)
        mul (recur
             (inc ctr)
             (assoc registers reg
                    (* (get registers reg 0)
                       (get-val registers arg)))
             last-played)
        add (recur
             (inc ctr)
             (assoc registers reg
                    (+ (get registers reg 0)
                       (get-val registers arg)))
             last-played)
        jgz (let [x-val (get registers reg 0)
                  y-val (get-val registers arg)
                  offset (if (zero? x-val)
                           1
                           y-val)]
              (recur (+ ctr offset)
                     registers
                     last-played))
        mod (recur (inc ctr)
                   (assoc registers reg
                          (mod (get registers reg 0)
                               (get-val registers arg)))
                   last-played)
        snd (let [v (get-val registers reg)]
              (recur (inc ctr)
                     registers
                     v))
        rcv last-played #_(recur (inc ctr)
                                 (assoc registers reg
                                        last-played)
                                 last-played)
        ))))

(defn part-1
  []
  (solve (data)))

(comment
  (peek (conj (clojure.lang.PersistentQueue/EMPTY) 1 2))
  (peek (pop (conj (clojure.lang.PersistentQueue/EMPTY) 1 2))))


(defn next-state
  [instructions {:keys [id ctr registers last-played buf0 buf1 waiting?] :as p}]
  (let [[instr reg arg] (get instructions ctr)]
    (case instr
      set (->
           p
           (update :ctr inc)
           (assoc  :registers reg (get-val registers arg)))
      mul (->
           p
           (update :ctr inc)
           (assoc  :registers reg (* (get registers reg 0)
                                     (get-val registers arg))))
      add (->
           p
           (update :ctr inc)
           (assoc  :registers reg (+ (get registers reg 0)
                                     (get-val registers arg))))
      jgz (let [x-val (get registers reg 0)
                y-val (get-val registers arg)
                offset (if (zero? x-val)
                         1
                         y-val)]
            (-> p
                (update :ctr + offset)))
      mod (-> p
              (update :ctr inc)
              (assoc :registers reg
                     (mod (get registers reg 0)
                          (get-val registers arg))))
      snd
      (let [v (get-val registers reg)]
        (->
         (if (zero? id)
           (update p
                   :buf0
                   conj v)
           (update p
                   :buf1
                   conj v))
         (update :ctr inc)))
      rcv
      (let [[v new-buf]
            (if (zero? id)
              [(peek buf1) (pop buf1)]
              [(peek buf0) (pop buf0)])]
        (if v
          (->
           (if (zero? id)
             (assoc p :buf1 new-buf)
             (assoc p :buf2 new-buf))
           (assoc :registers reg v
                  :waiting?  false))
          (assoc p :waiting? true))
        )
      )))

(defn solve2
  [instructions]
  (loop [{:keys [id ctr registers last-played buf0 waiting?] :as p0}
         {:id 0
          :ctr 0
          :registers {'p 0}
          :last-played nil
          :buf0 (clojure.lang.PersistentQueue/EMPTY)
          :waiting? false}
         {:keys [id ctr registers last-played buf1 waiting?] :as p1}
         {:id 2
          :ctr 0
          :registers {'p 0}
          :last-played nil
          :buf1 (clojure.lang.PersistentQueue/EMPTY)
          :waiting? false}]
    (cond
      (and (:waiting? p0) (:waiting? p1))
      ;; TODO
      (count buf0)
      (:waiting? p0)
      (recur (next-state instructions
                         (assoc p0 :buf1 buf1))
             p1)
      (:waiting? p1)
      (recur p0
             (next-state instructions
                         (assoc p0 :buf0 buf0)))
      :else
      (recur
       (next-state instructions
                   p0)
       (next-state instructions
                   p1))
      )
    ))

(defn part-2
  [])

;;;; Scratch

(comment
  (set! *print-length* 20)
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  (time (part-1)) ;; 3188, 2ms 
  (time (part-2)) 
  (quick-bench (part-2)) 
  )
