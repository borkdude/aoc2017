(ns util
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io BufferedReader]))

(defn input [f]
  (-> f
      io/resource
      slurp
      str/trim))

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
