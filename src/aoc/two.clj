(ns aoc.two
  (:require [clojure.string :as str]))

(defn step
  ([program]
   (step 0 program))
  ([pos program]
   (let [[op pos1 pos2 dest] (drop pos program)
         val1 (get program pos1)
         val2 (get program pos2)]
     (cond
       (= 1 op) (recur (+ pos 4) (assoc program dest (+ val1 val2)))
       (= 2 op) (recur (+ pos 4) (assoc program dest (* val1 val2)))
       (= 99 op) program
       :else program))))

(def output 19690720)

(defn find-output [output search-size]
  (reduce (fn [_ [noun verb]]
            (let [result (first (step (assoc processed 1 noun 2 verb)))]
              (when (= output result)
                (reduced [(+ (* 100 noun) verb) {:result result :noun noun :verb verb}]))))
          (for [noun (range search-size) verb (range search-size)]
            [noun verb])))

(comment
  (def raw (slurp "resources/2.txt"))
  (def processed (mapv #(Integer/parseInt %)
                       (str/split (str/trim-newline raw) #",")))

  (take 4 (drop 3 (range 10)))(3 4 5 6)

  (assoc [0 1 2 3] 3 99)[0 1 2 99]

  (step [1,9,10,3,2,3,11,0,99,30,40,50])
  [3500 9 10 70 2 3 11 0 99 30 40 50]

  [33 9 10 19 2 3 11 0 99 30 40 50]

  (step processed)
  [234699 0 0 2 1 1 2 3 1 3 4 3 1 5 0 3 2 13 1 0 1 9 19 3 2 23 13 15 1 27 9 18 2 31 6 36 1 5 35 37 1 10 39 41 2 43 6 82 1 10 47 86 2 6 51 172 1 5 55 173 1 59 9 176 1 13 63 181 2 6 67 362 1 5 71 363 2 6 75 726 2 79 6 1452 1 13 83 1457 1 9 87 1460 1 9 91 1463 1 5 95 1464 1 5 99 1465 2 13 103 7325 1 6 107 7327 1 9 111 7330 2 6 115 14660 1 13 119 14665 1 123 6 14667 1 127 5 14668 2 10 131 58672 2 135 10 234688 1 13 139 234693 1 10 143 234697 1 2 147 234697 1 6 151 0 99 2 14 0 0]

  (step (assoc processed 1 12 2 2))
  [3306701 12 2 2 1 1 2 3 1 3 4 3 1 5 0 3 2 13 1 60 1 9 19 63 2 23 13 315 1 27 9 318 2 31 6 636 1 5 35 637 1 10 39 641 2 43 6 1282 1 10 47 1286 2 6 51 2572 1 5 55 2573 1 59 9 2576 1 13 63 2581 2 6 67 5162 1 5 71 5163 2 6 75 10326 2 79 6 20652 1 13 83 20657 1 9 87 20660 1 9 91 20663 1 5 95 20664 1 5 99 20665 2 13 103 103325 1 6 107 103327 1 9 111 103330 2 6 115 206660 1 13 119 206665 1 123 6 206667 1 127 5 206668 2 10 131 826672 2 135 10 3306688 1 13 139 3306693 1 10 143 3306697 1 2 147 3306699 1 6 151 0 99 2 14 0 0]

  (reduce (fn [_ [noun verb]]
            (let [result (first (step (assoc processed 1 noun 2 verb)))]
              (when (= 19690720 result)
                (reduced [(+ (* 100 noun) verb) {:result result :noun noun :verb verb}]))))
          (for [noun (range 10) verb (range 10)]
            [noun verb]))
  nil

  (find-output output (count processed))
  [7621 {:result 19690720, :noun 76, :verb 21}]


  )
