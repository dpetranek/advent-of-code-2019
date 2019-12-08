(ns aoc.five)

(defn parse-instruction [n]
  (let [sn (str n)
        op (Integer/parseInt (subs sn (- (count sn) 2)))
        [p1-mode p2-mode p3-mode] (->> (subs sn 0 (- (count sn) 2))
                                       (reverse)
                                       (map #(Integer/parseInt (str %))))]
    {:op op
     :p1-mode (or p1-mode 0)
     :p2-mode (or p2-mode 0)
     :p3-mode (or p3-mode 0)}))

(def IMMEDIATE_MODE 1)

(defn read-val [mode pos]
  )

(comment
  (parse-instruction 1002)
  {:op 2, :p1-mode 0, :p2-mode 1, :p3-mode 0}
  )



(defn step
  ([program]
   (step 0 program))
  ([pos program]
   (let [{:keys [op]} (parse-instruction (get program pos))]
     (cond
       (= 1 op)
       (let [[op pos1 pos2 dest] (drop pos program)
             val1 (get program pos1)
             val2 (get program pos2)]
         (recur (+ pos 4) (assoc program dest (+ val1 val2))))

       (= 2 op)
       (let [[op pos1 pos2 dest] (drop pos program)
             val1 (get program pos1)
             val2 (get program pos2)]
         (recur (+ pos 4) (assoc program dest (* val1 val2))))

       (= 3 op)
       (recur (+ pos 2) (assoc program ))

       (= 99 op) program))))
