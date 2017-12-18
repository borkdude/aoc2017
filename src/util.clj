(ns util
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
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

(let [last-msg (atom nil)]
  (defn print-changed [& args]
    (when (not= args @last-msg)
      (apply println args)
      (reset! last-msg args))))
