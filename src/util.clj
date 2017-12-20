(ns util
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s])
  (:import [java.io BufferedReader]))

(defn read-lines
  [f]
  (-> f
      io/resource
      slurp
      str/trim
      (str/split #"\n")))

(defn read-first
  [f]
  (first (read-lines f)))

(defn lines-reducible [^BufferedReader rdr]
  (reify clojure.lang.IReduceInit
    (reduce [this f init]
      (try
        (loop [state init]
          (if (reduced? state)
            @state
            (if-let [line (.readLine rdr)]
              (recur (f state line))
              state)))
        (finally
          (.close rdr))))))

(defn resource-reducible [path]
  (let [rdr (io/reader (io/resource path))]
    (lines-reducible rdr)))

(defn parse-int
  ([s]
   (parse-int s 10))
  ([s b]
   (try (Integer/parseInt s b)
        (catch Exception e nil))))

(defn find-first
  [pred vals]
  (reduce
   (fn [_ v]
     (when (pred v)
       (reduced v)))
   nil
   vals))

(defn spy
  [x]
  (println x)
  x)

(let [last-msg (atom {})]
  (defn print-changed [k & args]
    (when (not= args (get @last-msg k))
      (apply println k args)
      (swap! last-msg assoc k args)))
  (defn reset-print-changed! []
    (reset! last-msg {})))

(defn take-until
  ([pred coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (cons (first s) nil)
        (cons (first s) (take-until pred (rest s))))))))

(defn assert!
  [spec expr]
  (if-not (s/valid? spec expr)
    (throw (ex-info "" (s/explain-data spec expr)))))
