(ns aoc.three
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(comment
  (def raw (slurp "resources/3.txt"))

  (def processed (map #(str/split % #",") (str/split-lines raw)))
  (def l1 (first processed))
  (def l2 (second processed))
  )

(defn interpret [[x y] instruction]
  (let [dir (first instruction)
        magnitude (Integer/parseInt (subs instruction 1))]
    (condp = dir
      \R [(+ x magnitude) y]
      \L [(- x magnitude) y]
      \U [x (+ y magnitude)]
      \D [x (- y magnitude)])))

(comment
  (interpret [0 0] "U3")
  [0 3]
  )

(defn instructions->coords [instructions]
  (reduce (fn [coords instruction]
            (conj coords (interpret (last coords) instruction)))
          [[0 0]] instructions))

(comment
  (reduce (fn [coords instruction]
            (conj coords (interpret (last coords) instruction)))
          [[0 0]]
          l1)

  (map instructions->coords processed)

  )

(defn intersection? [segment1 segment2]
  (->> (mapv (fn [[x1 y1] [x2 y2]] [(<= x1 x2) (<= y1 y2)]) segment1 segment2)
       (apply map not=)
       (= [true true])))

(comment
  ;; intersect
  (mapv (fn [[x1 y1] [x2 y2]] [(<= x1 x2) (<= y1 y2)])
        [[0 2] [2 2]]
        [[1 0] [1 2]])
  [[true false] [false true]]

  (mapv (fn [[x1 y1] [x2 y2]] [(<= x1 x2) (<= y1 y2)])
        [[1 0] [1 2]]
        [[0 1] [2 1]])
  [[false true] [true false]]

  ;; no intersect
  (mapv (fn [[x1 y1] [x2 y2]] [(<= x1 x2) (<= y1 y2)])
        [[0 2] [2 2]]
        [[1 0] [3 0]])
  [[true false] [true false]]

  (mapv (fn [[x1 y1] [x2 y2]] [(<= x1 x2) (<= y1 y2)])
        [[1 0] [1 2]]
        [[2 1] [2 2]])
  [[true true] [true true]]

  (mapv (fn [[x1 y1] [x2 y2]] [(<= x1 x2) (<= y1 y2)])
        [[3 0] [3 2]]
        [[2 1] [2 2]])
  [[false true] [false true]]

  (mapv (fn [[x1 y1] [x2 y2]] [(<= x1 x2) (<= y1 y2)])
        [[3 3] [3 1]]
        [[2 1] [2 2]])
  [[false false] [false true]]

  ;; intersection if xs are not equal and ys are not equal
  (apply map not= [[false false] [false true]])
  (false true)
  (= [true true] (apply map not= [[false false] [false true]]))
  false
  )



(defn segmentu->points [[[x1 y1] [x2 y2]]]
  (cond (= x1 x2) (let [ys (if (< y1 y2)
                             (take-while #(not= % (inc y2)) (iterate inc y1))
                             (take-while #(not= % (dec y2)) (iterate dec y1)))]
                    (map #(vector x1 %) ys))
        (= y1 y2) (let [xs (if (< x1 x2)
                             (take-while #(not= % (inc x2)) (iterate inc x1))
                             (take-while #(not= % (dec x2)) (iterate dec x1)))]
                    (map #(vector % y1) xs))
        :else     (throw (Exception. (str "Segment has invalid slope: " [[x1 y1] [x2 y2]])))))

(comment
  (segment->points [[0 2] [2 2]])
  ([0 2] [1 2] [2 2])
  (segment->points [[0 -2] [-2 -2]])
  ([0 -2] [-1 -2] [-2 -2])
  '(0 -1 -2)
  '()
  (range 0 (inc -2))
  ()
  (range 0 -2)
  ()

  (take-while #(not= % 1) (iterate dec 3))
  '(3 2)

  (let [x1 -5
        x2 5]
    (take-while #(not= % x2) (iterate (if (< x1 x2) inc dec) x1)))
  (-5 -4 -3 -2 -1 0 1 2 3 4)
  (let [x1 5
        x2 -5]
    (take-while #(not= % x2) (iterate (if (< x1 x2) inc dec) x1)))
  (5 4 3 2 1 0 -1 -2 -3 -4)
  )

(defn find-intersection [segment1 segment2]
  (when (not= segment1 segment2)
    (let [points1 (segment->points segment1)
          points2 (segment->points segment2)]
      (not-empty (set/intersection (set points1) (set points2))))))


(comment
  (let [[[x1 y1] [x2 y2]] [[0 0] [1 5]]]
    (cond (= x1 x2) (fn [] )
          (= y1 y2) :y-stays-same-throughout
          :else (throw (Exception. (str "Segment has invalid slope: " [[x1 y1] [x2 y2]])))))

  (find-intersection [[0 2] [2 2]] [[1 0] [1 2]])
  #{[1 2]}
  [#{[2 2] [0 2] [1 2]} #{[1 0] [1 1] [1 2]}]
  [([0 2] [1 2] [2 2]) ([1 0] [1 1] [1 2])]
  [(0 1 2) (0 1 2)]
  [#{0 1 2} #{0 1 2}]
  [#{0 1 2} #{0 1 2}]
  [#{[0 2]} #{[1 0] [1 1] [1 2]}]
  #{}

  (find-intersection [[3 3] [3 1]] [[2 1] [2 2]])
  nil



  )


(defn find-intersections [segment segments]
  (reduce (fn [intersections s]
            (if-let [cross (find-intersection segment s)]
              (set/union intersections cross)
              intersections))
          #{}
          segments))

(defn find-all-intersections [segments]
  (reduce (fn [intersections segment]
            (set/union intersections (find-intersections segment segments)))
          #{}
          segments))

(comment
  (->> (take 5 l1)
       (instructions->coords)
       (partition 2 1)
       (find-intersections '([0 0] [998 0])))



  (find-intersection '([0 0] [998 0]) '([0 0] [998 0]))


  (([0 0] [998 0]) ([998 0] [998 547]) ([998 547] [295 547]) ([295 547] [295 296]) ([295 296] [-481 296]) ([-481 296]))

  '(([0 0] [998 0]) ([998 0] [998 547]) ([998 547] [295 547]) ([295 547] [295 296]) ([295 296] [-481 296]) ([-481 296]))
  [[0 0] [998 0] [998 547] [295 547] [295 296] [-481 296]]
  '("R998" "U547" "L703" "D251" "L776")




  (time (->> (map instructions->coords processed) ; two colls of coords [[x y]...]
             (mapcat (partial partition 20 1)) ; one col of all segments [[[x1 y1] [x2 y2]]...]
             (find-all-intersections)
             (map (fn [[x y]] (+ (Math/abs x) (Math/abs y))))
             (reduce #(if (< %1 %2) %1 %2))))
  2
  "Elapsed time: 357296.147901 msecs"






  )
