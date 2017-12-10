(set-env!
 :dependencies '[[org.clojure/clojure "1.9.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [net.cgrand/xforms "0.15.0"]
                 [criterium "0.4.4"]
                 [instaparse "1.4.8"]]
 :resource-paths #{"resources" "src"})

(task-options!
 repl {:eval '(set! *print-length* 20)})

(deftask run-day
  [d day  VAL int "day"
   p part VAL int "part"]
  (assert day)
  (let [parts (if part
                [part]
                [1 2])]
    (doseq [p parts]
      (let [ns  (format "day%02d" day)
            sym (symbol ns
                        (str "part-" p))
            _   (require (symbol ns))
            fn  (resolve sym)]
        (println (str sym ":") (fn))))))

(deftask run-all
  []
  (with-pass-thru [_]
    (let [days (map #(format "day%02d" %)
                    (range 1 (inc 10)))]
      (doseq [ns days
              :let [_  (require (symbol ns))
                    p1 (resolve (symbol ns "part-1"))
                    p2 (resolve (symbol ns "part-2"))]]
        (println (str ns ":") [(p1) (p2)])))))
