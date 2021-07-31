(ns fp-guild-clojure-scratch.core (:gen-class))

(def raw-input (slurp "src/fp_guild_clojure_scratch/input.txt"))

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))


(defn GCContent? [sequenceNT]
  (let [
        groupedNT (group-by identity sequenceNT)
        NTcount (into {} (for [[k,v] groupedNT] [k (count v)]))
        GC (+ (NTcount \G 0) (NTcount \C 0))
        total (apply + (vals NTcount))
        ]
    (* (double (/ GC total)) 100)
    )
  )


(defn sequences-GC [in]
  (let [parsed-sequences (->> in
                           ((fn [x] (clojure.string/split x #">")))
                           (remove clojure.string/blank?)
                           (map (fn [x](clojure.string/split x #"\n" 2)))
                           (into {})
                           (map-vals (fn [x] (clojure.string/replace x "\n" "")))
                           )
        ]
    (map-vals GCContent? parsed-sequences)
    )
  )

(defn print-result []
  (println "Result is "(apply max-key val (sequences-GC raw-input)))
  )

(defn -main [& args]
  (print-result)
  )