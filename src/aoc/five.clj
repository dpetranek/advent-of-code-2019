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

(defn get-val [program mode pos]
  (if (= POSITION_MODE mode)
    (get program pos)
    pos))

(defn step
  ([program]
   (step 0 program))
  ([pos program]
   (let [{:keys [op p1-mode p2-mode p3-mode]} (parse-instruction (get program pos))]
     (cond
       ;; add
       (= 1 op)
       (let [[_ pos1 pos2 dest] (drop pos program)
             val1 (get-val program p1-mode pos1)
             val2 (get-val program p2-mode pos2)]
         (recur (+ pos 4) (assoc program dest (+ val1 val2))))

       ;; multiply
       (= 2 op)
       (let [[_ pos1 pos2 dest] (drop pos program)
             val1 (get-val program p1-mode pos1)
             val2 (get-val program p2-mode pos2)]
         (recur (+ pos 4) (assoc program dest (* val1 val2))))

       ;; read
       (= 3 op)
       (let [[_ pos1] (drop pos program)]
         (recur (+ pos 2) (assoc program pos1 (Integer/parseInt (read-line)))))

       ;; write
       (= 4 op)
       (let [[_ pos1] (drop pos program)]
         (println (get-val program p1-mode pos1))
         (recur (+ pos 2) program))

       ;; jump-if-true
       (= 5 op)
       (let [[_ pos1 pos2] (drop pos program)
             val1 (get-val program p1-mode pos1)
             val2 (get-val program p2-mode pos2)]
         (if (zero? val1)
           (recur (+ pos 3) program)
           (recur val2 program)))

       ;; jump-if-false
       (= 6 op)
       (let [[_ pos1 pos2] (drop pos program)
             val1 (get-val program p1-mode pos1)
             val2 (get-val program p2-mode pos2)]
         (if (zero? val1)
           (recur val2 program)
           (recur (+ pos 3) program)))

       ;; less than
       (= 7 op)
       (let [[_ pos1 pos2 pos3] (drop pos program)
             val1 (get-val program p1-mode pos1)
             val2 (get-val program p2-mode pos2)]
         (if (< val1 val2)
           (recur (+ pos 4) (assoc program pos3 1))
           (recur (+ pos 4) (assoc program pos3 0))))

       ;; equals
       (= 8 op)
       (let [[_ pos1 pos2 pos3] (drop pos program)
             val1 (get-val program p1-mode pos1)
             val2 (get-val program p2-mode pos2)]
         (if (= val1 val2)
           (recur (+ pos 4) (assoc program pos3 1))
           (recur (+ pos 4) (assoc program pos3 0))))

       ;; end
       (= 99 op) program

       ;; error
       :else {:error true
              :pos pos
              :program program}))))

(comment
  (def raw (slurp "resources/5.txt"))

  (def processed (mapv #(Integer/parseInt %)
                       (str/split (str/trim-newline raw) #",")))

  (step processed)

  (step [3,9,8,9,10,9,4,9,99,-1,8])
  (step [3,9,7,9,10,9,4,9,99,-1,8])
  (step [3,3,1108,-1,8,3,4,3,99])
  (step [3,3,1107,-1,8,3,4,3,99])
  (step [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9])
  (step [3,3,1105,-1,9,1101,0,0,12,4,12,99,1])
  (step [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
         1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
         999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99])


  (step [1002,4,3,4,33])

  )
