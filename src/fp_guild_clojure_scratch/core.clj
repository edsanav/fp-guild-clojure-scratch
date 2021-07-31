(ns fp-guild-clojure-scratch.core (:gen-class))

(def raw-input (slurp "src/fp_guild_clojure_scratch/input.txt"))

(defn fvals [m f]
  (into {} (for [[k v] m] [k (f v)])))


(defn by-base [ltt]
  (vector (first ltt), (count (nth ltt 1)))
  )



(defn GCContent? [sequenceNT]
  (let [
        groupedNT (into {} (map by-base (group-by identity sequenceNT)))
        GC (+ (groupedNT \G) (groupedNT \C))
        total (apply + (vals groupedNT))
        ]
    (* (double (/ GC total)) 100)
    )
  )


(defn sequences-GC [in]
  (let [raw-sequences (->> in
                           (#(clojure.string/split % #">"))
                           (remove clojure.string/blank?)
                           (map #(clojure.string/split % #"\n" 2))
                           (into {})
                           )
        final-sequences (fvals raw-sequences (fn [x] (clojure.string/replace x "\n" "")))
        ]
    (fvals final-sequences GCContent?)
    )
  )

(defn print-result []
  (println "Result is "(apply max-key val (sequences-GC raw-input)))
  )

(defn -main [& args]
  (print-result)
  )