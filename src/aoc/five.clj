(ns aoc.five
  (:require [clojure.string :as str]))

(def IMMEDIATE_MODE 1)
(def POSITION_MODE 0)

(defn parse-instruction [n]
  (let [sn (str n)
        op (Integer/parseInt (subs sn (max (- (count sn) 2) 0)))
        [p1-mode p2-mode p3-mode] (->> (subs sn 0 (max (- (count sn) 2) 0))
                                       (reverse)
                                       (map #(Integer/parseInt (str %))))]
    {:op op
     :p1-mode (or p1-mode POSITION_MODE)
     :p2-mode (or p2-mode POSITION_MODE)
     :p3-mode (or p3-mode POSITION_MODE)}))

(comment
  (parse-instruction 3)
  {:op 3, :p1-mode 0, :p2-mode 0, :p3-mode 0}
  (parse-instruction 1002)
  {:op 2, :p1-mode 0, :p2-mode 1, :p3-mode 0}

  )

(defn step
  ([program]
   (step 0 program))
  ([pos program]
   (let [{:keys [op p1-mode p2-mode p3-mode]} (parse-instruction (get program pos))]
     (cond
       ;; add
       (= 1 op)
       (let [[_ pos1 pos2 dest] (drop pos program)
             val1 (if (= POSITION_MODE p1-mode)
                    (get program pos1)
                    pos1)
             val2 (if (= POSITION_MODE p2-mode)
                    (get program pos2)
                    pos2)]
         (recur (+ pos 4) (assoc program dest (+ val1 val2))))
       ;; multiply
       (= 2 op)
       (let [[_ pos1 pos2 dest] (drop pos program)
             val1 (if (= POSITION_MODE p1-mode)
                    (get program pos1)
                    pos1)
             val2 (if (= POSITION_MODE p2-mode)
                    (get program pos2)
                    pos2)]
         (recur (+ pos 4) (assoc program dest (* val1 val2))))
       ;; read
       (= 3 op)
       (let [[_ pos1] (drop pos program)]
         (recur (+ pos 2) (assoc program pos1 (Integer/parseInt (read-line)))))
       ;; write
       (= 4 op)
       (let [[_ pos1] (drop pos program)]
         (println (get program pos1))
         (recur (+ pos 2) program))

       (= 99 op) program
       :else {:error true
              :pos pos
              :program program}))))

(comment
  (def raw (slurp "resources/5.txt"))

  (def processed (mapv #(Integer/parseInt %)
                       (str/split (str/trim-newline raw) #",")))

  (step processed)


  (step [1002,4,3,4,33])

  )
