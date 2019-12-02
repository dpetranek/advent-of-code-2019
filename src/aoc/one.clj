(ns aoc.one
  (:require [clojure.string :as str]))

(defn calc-fuel [mass]
  (- (Math/floor (/ mass 3)) 2))

(defn calc-fuel2
  ([mass]
   (let [required (- (Math/floor (/ mass 3)) 2)]
     (if (<= required 0)
       required
       (calc-fuel2 required required))))
  ([fuel total]
   (let [required (- (Math/floor (/ fuel 3)) 2)]
     (if (<= required 0)
       total
       (recur required (+ total required))))))

(comment
  (def raw (mapv #(Integer/parseInt %)
                 (str/split-lines (slurp "resources/1.txt"))))

  (->> (map calc-fuel raw)
       (reduce +))
  3353880.0

  (calc-fuel2 100756)

  (->> (map calc-fuel2 raw)
       (reduce +))
  5027950.0)
