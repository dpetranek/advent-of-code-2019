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
  ;; load the input
  (def raw (slurp "resources/2.txt"))
  ;; turn it into clojure datastructures
  (def processed (mapv #(Integer/parseInt %)
                       (str/split (str/trim-newline raw) #",")))

  ;; I can drop the pos to get the current instruction set
  (take 4 (drop 3 (range 10)))(3 4 5 6)
  (drop 0 (range 10))(0 1 2 3 4 5 6 7 8 9)
  (drop 4 (range 10))
  (4 5 6 7 8 9)

  ;; vectors are associative - nice immutable updates
  (assoc [0 1 2 3] 3 99)[0 1 2 99]

  ;; test to make sure it works
  (step [1,9,10,3,2,3,11,0,99,30,40,50])
  [3500 9 10 70 2 3 11 0 99 30 40 50]

  ;; test with actual input
  (step processed)
  [234699 0 0 2 1 1 2 3 1 3 4 3 1 5 0 3 2 13 1 0 1 9 19 3 2 23 13 15 1 27 9 18 2 31 6 36 1 5 35 37 1 10 39 41 2 43 6 82 1 10 47 86 2 6 51 172 1 5 55 173 1 59 9 176 1 13 63 181 2 6 67 362 1 5 71 363 2 6 75 726 2 79 6 1452 1 13 83 1457 1 9 87 1460 1 9 91 1463 1 5 95 1464 1 5 99 1465 2 13 103 7325 1 6 107 7327 1 9 111 7330 2 6 115 14660 1 13 119 14665 1 123 6 14667 1 127 5 14668 2 10 131 58672 2 135 10 234688 1 13 139 234693 1 10 143 234697 1 2 147 234697 1 6 151 0 99 2 14 0 0]

  ;; test what the problem says
  (first (step (assoc processed 1 12 2 2)))
  3306701

  ;; test a brute force search
  (reduce (fn [_ [noun verb]]
            (let [result (first (step (assoc processed 1 noun 2 verb)))]
              (when (= 19690720 result)
                (reduced [(+ (* 100 noun) verb) {:result result :noun noun :verb verb}]))))
          (for [noun (range 10) verb (range 10)]
            [noun verb]))
  nil

  ;; don't want to reach out-of-bounds, so limit search to the length of the program
  (find-output output (count processed))
  [7621 {:result 19690720, :noun 76, :verb 21}]


  )
