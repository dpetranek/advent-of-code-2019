(ns aoc.three
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(comment
  (def raw (slurp "resources/3.txt"))

  (def processed (map #(str/split % #",") (str/split-lines raw)))
  (def l1 (first processed))
  (def l2 (second processed))

  l1
  )

(defn find-next-point [[x y] instruction]
  (let [dir (first instruction)
        magnitude (Integer/parseInt (subs instruction 1))]
    (condp = dir
      \R [(+ x magnitude) y]
      \L [(- x magnitude) y]
      \U [x (+ y magnitude)]
      \D [x (- y magnitude)])))

(defn instructions->coords [instructions]
  (reduce (fn [coords instruction]
            (conj coords (find-next-point (last coords) instruction)))
          [[0 0]] instructions))

(defn orientation [segment]
  (let [[[x1 y1] [x2 y2]] segment]
     (if (= x1 x2) :vertical :horizontal)))

(defn intersection? [segment1 segment2]
  (->> (mapv (fn [[x1 y1] [x2 y2]] [(<= x1 x2) (<= y1 y2)]) segment1 segment2)
       (apply map not=)
       (= [true true])))

(defn segment->points [segment]
  (let [[[x1 y1] [x2 y2]] segment]
    (case (orientation segment)
      :horizontal
      (set (map #(vector % y1) (apply range (if (< x1 x2)
                                              [x1 (inc x2)]
                                              [x2 (inc x1)]))))
      :vertical
      (set (map #(vector x1 %) (apply range (if (< y1 y2)
                                              [y1 (inc y2)]
                                              [y2 (inc y1)])))))))

(defn find-intersection
  [segment1 segment2]
  (let [v-points (segment->points segment1)
        h-points (segment->points segment2)]
    (set/intersection v-points h-points)))

(defn find-intersections
  [line1 line2]
  (let [[{horizontal1 :horizontal vertical1 :vertical}
         {horizontal2 :horizontal vertical2 :vertical}]
        (->> [line1 line2]
             (map instructions->coords)
             (map (partial partition 2 1))
             (map (partial group-by orientation))
             (map (partial into {} (map (juxt key (comp set val))))))]
    (loop [[h1-seg & h1-segs] horizontal1
           [h2-seg & h2-segs] horizontal2
           all-intersections #{}]
      (if (and h1-seg h2-seg)
        (let [h1v2 (reduce (fn [intersections v-seg]
                             (if (intersection? v-seg h1-seg)
                               (set/union intersections (find-intersection v-seg h1-seg))
                               intersections))
                           #{}
                           vertical2)
              h2v1 (reduce (fn [intersections v-seg]
                             (if (intersection? v-seg h2-seg)
                               (set/union intersections (find-intersection v-seg h2-seg))
                               intersections))
                           #{}
                           vertical1)]
          (recur h1-segs h2-segs (set/union all-intersections h1v2 h2v1)))
        all-intersections))))

(defn manhatten-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(comment
  (def xl10 ["U1" "R2"])
  (def xl20 ["R1" "U2"])
  (def xl1 [[[0 0] [1 0]] [[1 0] [1 3]]])
  (def xl2 [[[0 0] [0 1]] [[0 1] [3 1]]])

  (orientation [[0 2] [2 2]])
  :horizontal
  (orientation [[2 0] [2 2]])
  :vertical

  (set (mapcat segment->points xl1))
  #{[0 0] [1 0] [1 1] [1 3] [1 2]}
  (set (mapcat segment->points xl2))
  #{[0 0] [1 1] [3 1] [2 1] [0 1]}
  (set/intersection (set (mapcat segment->points xl1))
                    (set (mapcat segment->points xl2)))
  #{[0 0] [1 1]}

  (let [{:keys [horizontal vertical] :as segments} (->> (instructions->coords xl10)
                                                        (partition 2 1)
                                                        (group-by orientation))]
    segments)
  {:vertical [([0 0] [0 1])], :horizontal [([0 1] [2 1])]}
  {:horizontal [([0 0] [0 1])], :vertical [([0 1] [2 1])]}

  (into {} (map (juxt key val)) {:a 1})



  (into {} (map (juxt key (comp set val))) {:vertical [[[0 0] [1 0]]], :horizontal [[[1 0] [1 3]]]})
  {:vertical #{[[0 0] [1 0]]}, :horizontal #{[[1 0] [1 3]]}}
  {:vertical #{:vertical [[[0 0] [1 0]]]}, :horizontal #{:horizontal [[[1 0] [1 3]]]}}

  (->> [xl10 xl20]
       (map instructions->coords)
       (map (partial partition 2 1)))
  ((([0 0] [0 1]) ([0 1] [2 1])) (([0 0] [1 0]) ([1 0] [1 2])))
  ([[0 0] [0 1] [2 1]] [[0 0] [1 0] [1 2]])


  (let [{:keys [horizontal vertical]} (->> [xl1 xl2]
                                           (map (partial group-by horizontal?))
                                           (map (partial into {} (map (juxt key (comp set val)))))
                                           (reduce (partial merge-with set/union)))]
    (reduce (fn [all-intersections h-seg]
              (set/union all-intersections
                         (reduce (fn [intersections v-seg]
                                   (if (intersection? v-seg h-seg)
                                     (let [v-points (segment->points v-seg)
                                           h-points (segment->points h-seg)]
                                       (set/union intersections (set/intersection v-points h-points)))
                                     intersections))
                                 #{}
                                 vertical)))
            #{}
            horizontal))

  (find-intersections xl10 xl20)
  #{[1 1]}

  (find-intersections l1 l2)
  #{[-481 742] [-404 1270] [-943 0] [99 2038] [-199 1373] [-381 1362] [-412 1819] [-199 1819] [-481 592] [-412 1763] [-404 1362] [-435 296] [-450 296] [-1739 2069] [-381 1270] [-481 329] [-943 122] [-1739 1911] [-184 1270] [-364 2619] [-481 526]}
  #{}

  (->> (find-intersections l1 l2)
       (map manhatten-distance)
       (sort)
       (first))
  731
  )
