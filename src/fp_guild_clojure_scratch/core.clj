(ns fp-guild-clojure-scratch.core
  (:gen-class))

(def raw-input (slurp (clojure.java.io/resource "input.txt")))

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))


(defn GCContent? [sequenceNT]
  (let [
        groupedNT (group-by identity sequenceNT)
        NTcount (into {} (for [[k, v] groupedNT] [k (count v)]))
        GC (+ (NTcount \G 0) (NTcount \C 0))
        total (apply + (vals NTcount))
        ]
    (* (double (/ GC total)) 100)
    )
  )

(defn parse-sequences [in]
  (->> in
       ((fn [x] (clojure.string/split x #">")))
       (remove clojure.string/blank?)
       (map (fn [x] (clojure.string/split x #"\n" 2)))
       (into {})
       (map-vals (fn [x] (clojure.string/replace x "\n" "")))
       )
  )


(defn sequences-GC [in]
  (map-vals GCContent? (parse-sequences in)
            )
  )

(defn print-result []
  (println "Result is " (apply max-key val (sequences-GC raw-input)))
  )

(defn -main [& args]
  (print-result)
  )

(comment
  "As improved by Dani"
  (defn gc-content [dna]
    (let [freqs (frequencies dna)
          total (count dna)]
      (float
        (* 100
           (/
             (+ (get freqs \G 0)
                (get freqs \C 0))
             total)))))


  (defn problem-1 [file]
    (let [txt-input (slurp (clojure.java.io/resource file))
          lines (clojure.string/split txt-input #">")
          cases (map clojure.string/split-lines (rest lines))
          solutions (for [case cases]
                      (let [id (first case)
                            dna (apply str (rest case))
                            gc (gc-content dna)] {id gc}))]
      (apply max-key val (into {} solutions))))
  )