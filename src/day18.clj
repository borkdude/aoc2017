(ns day18
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [criterium.core :refer [quick-bench]]
   [util :refer [resource-reducible
                 parse-int find-first
                 print-changed]]))

(defn data
  []
  (into []
        (comp
         (map #(format "[%s]" %))
         (map edn/read-string))
        (resource-reducible "day18.txt")))

(defn get-val
  [registers reg]
  (if (symbol? reg)
    (get registers reg 0) reg))

(defn assoc-reg
  [prog reg val]
  (update prog :registers assoc reg val))

(defn solve1
  [instructions]
  (loop [{:keys [ctr
                 registers
                 recovered?]
          :as p}
         {:ctr 0
          :registers {}
          :recovered? false
          :last-played nil}]
    (if recovered?
      p
      (recur
       (let [[instr reg arg] (get instructions ctr)
             p (update p :ctr inc)]
         (case instr
           set (assoc-reg p reg (get-val registers arg))
           mul (assoc-reg p reg (* (get registers reg 0)
                                   (get-val registers arg)))
           add (assoc-reg p reg (+ (get registers reg 0)
                                   (get-val registers arg)))
           jgz (let [x-val (get-val registers reg)
                     y-val (get-val registers arg)
                     offset (if (zero? x-val)
                              1
                              y-val)]
                 (assoc p :ctr (+ ctr offset)))
           mod
           (assoc-reg p reg (mod (get registers reg 0)
                                 (get-val registers arg)))
           snd (assoc p :last-played (get-val registers reg))
           rcv (assoc p :recovered? true)))))))

(defn part-1
  []
  (:last-played (solve1 (data))))

;; temp hack
(def p1-sending (atom 0))

(defn next-state
  [instructions {:keys [id ctr registers in out waiting?] :as p}]
  (let [[instr reg arg] (get instructions ctr)
        p (update p :ctr inc)]
    (case instr
      set (assoc-reg p reg (get-val registers arg))
      mul (assoc-reg p reg (* (get registers reg 0)
                              (get-val registers arg)))
      add (assoc-reg p reg (+ (get registers reg 0)
                              (get-val registers arg)))
      jgz (let [x-val (get-val registers reg)
                y-val (get-val registers arg)
                offset (if (zero? x-val)
                         1
                         y-val)]
            (assoc p :ctr (+ ctr offset)))
      mod
      (assoc-reg p reg (mod (get registers reg 0)
                            (get-val registers arg)))
      snd
      (let [v (get-val registers reg)]
        ;; temp hack to count p1 sending
        (when (= id 1)
          (swap! p1-sending inc))
        (update p :out conj v))
      rcv (if-let [v (peek in)]
            (->
             (assoc p
                    :in (pop in)
                    :waiting? false)
             (assoc-reg reg v))
            (assoc p
                   :waiting? true
                   :ctr ctr)))))

(defn loop-until-wait
  [instructions prog]
  (loop [p (next-state instructions prog)]
    (if (:waiting? p)
      p
      (recur (next-state instructions p)))))

(defn print-qs [& qs]
  (map #(pr-str (into [] %)) qs))

(defn next-state'
  [instructions iter progs]
  (let [[p0 p1] progs
        ;;_ (println "p0 in out" (print-qs (:in p0) (:out p0)))
        ;;_ (println "p1 in out" (print-qs (:in p1) (:out p1)))
 
        p0 (loop-until-wait instructions p0)
        _ (assert (empty? (:in p1)))
        ;;_ (println "p0 in out" (print-qs (:in p0) (:out p0)))
        ;;_ (println "p1 in out" (print-qs (:in p1) (:out p1)))

        ;; transfer p0 out to p1 in
        p1 (assoc p1 :in (:out p0))
        p0 (assoc p0 :out (clojure.lang.PersistentQueue/EMPTY))
        ;;_ (println "after transfer")
        ;;_ (println "p0 in out" (print-qs (:in p0) (:out p0)))
        ;;_ (println "p1 in out" (print-qs (:in p1) (:out p1)))
        
        p1 (loop-until-wait instructions p1)
        _ (assert (empty? (:in p0)))

        ;; transfer p1 out to p0 in, for next iteration       
        p0 (assoc p0 :in (:out p1))
        p1 (assoc p1 :out (clojure.lang.PersistentQueue/EMPTY))

        ;;_ (println "after transfer")
        ;;_ (println "p0 in out" (print-qs (:in p0) (:out p0)))
        ;;_ (println "p1 in out" (print-qs (:in p1) (:out p1)))
        ]
    [p0 p1]))

(defn program
  [id]
  {:id id
   :ctr 0
   :registers {'p id}
   :in (clojure.lang.PersistentQueue/EMPTY)
   :out (clojure.lang.PersistentQueue/EMPTY) 
   :waiting? false})

(defn solve2
  [instructions]
  (loop [iters 0
         progs
         [(program 0)
          (program 1)]]
    (let [[p0 p1] progs]
      (if (or #_(> iters 1 #_500000000)
              (and (:waiting? p0)
                   (:waiting? p1)
                   (empty? (:in p0))
                   (empty? (:in p1))
                   (empty? (:out p0))
                   (empty? (:out p1))))
        [iters
         (:waiting? p0)
         (:waiting? p1)
         (count (:in p0))
         (count (:in p1))
         (count (:out p0))
         (count (:out p1))]
        (do
          (recur
           (inc iters)
           (next-state' instructions iters progs)))))))

(def example-data
  '[[snd 1]
    [snd 2]
    [snd p]
    [rcv a]
    [rcv b]
    [rcv c]
    [rcv d]])

(defn part-2
  []
  (do
    ;; temp hack for now
    (reset! p1-sending 0)
    (solve2 (data)))
  @p1-sending ;; 127, too low, should be around 7000-8000
  ) 

;;;; Scratch

(comment
  (set! *print-length* 20)
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  (time (part-1)) ;; 3188, 2ms 
  (time (part-2)) ;; 127, incorrect 
  )
