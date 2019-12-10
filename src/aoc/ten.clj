(ns aoc.ten
  (:require [clojure.string :as str]))

(defn raw->data [raw]
  (mapv (comp vec seq) (str/split-lines raw)))

(defn slope [origin target]
  (let [[x1 y1] origin
        [x2 y2] target]
    (if (= x1 x2)
      999999999999999999999
      (/ (- y2 y1) (- x2 x1)))))

(defn distance [origin target]
  (let [[x1 y1] origin
        [x2 y2] target]
    (+ (Math/abs (- x1 x2))
       (Math/abs (- y1 y2)))))

(comment
  (distance [0 0] [1 1])
  2
  (distance [0 0] [2 1])
  3
  ;; 45deg
  (slope [0 0] [1 1])
  1
  (slope [0 0] [-1 1])
  -1
  ;; vertical
  (slope [0 0] [0 1])

  ;; horizontal
  (slope [0 0] [1 0])
  0
  )

(defn assign-coordinates [data]
  (map-indexed
   (fn [y row]
     (map-indexed
      (fn [x a]
        [x y a])
      row))
   data))

(defn filter-asteroids [coords]
  (mapcat (partial keep (fn [[x y a]] (when (= a \#) [x y]))) coords))


(defn count-astroids [field vantage]
  (let [[x1 y1] vantage]
    (->> field
         (map (fn [[x2 y2]]
                   [(slope [x1 y1] [x2 y2])
                    (distance [x1 y1] [x2 y2])
                    [x2 y2]]))
         (group-by first)
         (vals)
         (map (partial sort-by second))
         (map first)
         (map last)
         (count))))

(defn find-best-vantage [raw]
  (let [data     (raw->data raw)
        field    (assign-coordinates data)
        astroids (filter-asteroids field)]
    (->> astroids
         (map #(vector % (count-astroids astroids %)))
         (sort-by second)
         (last)
         )))


(comment
  (def raw (slurp "resources/10.txt"))

  (def test1 ".#..#
.....
#####
....#
...##")
  (def test2 "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")
  (def data1 (raw->data test1))
  (def coords1 (assign-coordinates data1))
  (def asteroids1 (mapcat (partial keep (fn [[x y a]] (when (= a \#) [x y]))) coords1))
  coords1
  asteroids1


  (let [field (assign-coordinates data1)
        astroids (filter-asteroids field)]
    (->> astroids
         (map #(vector % (count-astroids astroids %)))
         (sort-by second)
         (last)))

  (find-best-vantage test1)
  [3 4]
  (find-best-vantage test2)
  [[0 1] 32]
  [0 1]

  data1
  [[\. \# \. \. \#]
   [\. \. \. \. \.]
   [\# \# \# \# \#]
   [\. \. \. \. \#]
   [\. \. \. \# \#]]
  (map-indexed
   (fn [y row]
     (map-indexed
      (fn [x a]
        [x y a])
      row))
   data1)
  (mapcat
   (partial map (fn [[x y a]] [a]))
   '(([0 0 \.] [1 0 \#] [2 0 \.] [3 0 \.] [4 0 \#]) ([0 1 \.] [1 1 \.] [2 1 \.] [3 1 \.] [4 1 \.]) ([0 2 \#] [1 2 \#] [2 2 \#] [3 2 \#] [4 2 \#]) ([0 3 \.] [1 3 \.] [2 3 \.] [3 3 \.] [4 3 \#]) ([0 4 \.] [1 4 \.] [2 4 \.] [3 4 \#] [4 4 \#])))



  [[nil \# nil nil \#]
   [nil nil nil nil nil]
   [\# \# \# \# \#]
   [nil nil nil nil \#]
   [nil nil nil \# \#]]




  [[\# \. \. \# \. \# \. \# \. \# \# \# \# \# \# \. \. \# \. \# \. \. \. \# \#]
   [\# \# \. \# \. \. \# \. \# \. \. \# \# \. \# \. \. \# \# \# \# \# \# \. \#]
   [\. \# \. \# \# \. \# \. \. \# \# \. \. \# \. \# \. \# \# \# \# \. \# \. \.]
   [\. \# \. \. \# \# \. \# \. \# \. \. \# \. \# \. \. \. \# \. \. \. \# \. \#]
   [\# \. \. \. \# \# \# \. \# \# \. \# \# \. \. \# \# \. \. \. \# \. \. \# \.]
   [\# \# \. \. \# \. \# \. \# \. \# \# \# \. \. \. \# \. \# \# \. \. \# \. \#]
   [\# \# \# \. \# \# \# \. \# \. \# \# \. \# \# \. \. \. \. \# \# \# \# \# \.]
   [\. \# \# \# \# \# \. \# \. \# \. \. \. \# \. \. \# \# \# \# \# \. \. \# \.]
   [\. \# \. \# \# \. \. \. \# \. \# \. \. \. \# \# \# \# \# \. \# \# \. \. \.]
   [\# \# \# \# \# \# \. \# \. \. \# \# \. \# \. \. \# \. \# \. \# \. \. \. \.]
   [\# \# \# \. \# \# \. \# \# \# \# \# \# \# \. \. \. \. \# \# \. \# \. \. \#]
   [\. \# \# \# \# \. \# \# \. \. \# \. \# \# \. \# \. \# \. \# \# \. \. \. \#]
   [\# \# \. \. \. \# \# \. \# \# \# \# \# \# \. \. \# \# \. \. \# \. \# \# \#]
   [\. \. \. \# \# \# \. \. \. \# \. \. \# \. \. \. \# \. \# \# \# \. \. \# \.]
   [\. \# \# \# \# \# \. \. \. \# \# \. \. \# \. \. \# \# \# \# \# \. \# \# \#]
   [\. \# \# \# \# \# \. \. \# \. \# \# \# \# \# \# \# \. \# \# \# \. \# \# \.]
   [\# \. \. \. \# \# \# \. \# \# \# \# \. \# \# \. \# \# \. \# \. \# \# \. \#]
   [\. \# \. \# \. \# \. \# \. \# \. \# \# \. \# \. \. \# \. \# \. \. \# \# \#]
   [\# \# \. \# \. \# \# \# \# \. \# \# \# \. \. \. \. \# \# \# \. \. \# \# \.]
   [\# \. \. \# \# \. \# \. \. \. \. \# \. \. \# \. \. \# \. \# \. \. \# \. \#]
   [\# \# \. \. \# \. \. \# \. \. \. \# \. \. \# \# \. \. \# \# \# \# \. \. \#]
   [\. \. \. \. \# \. \. \. \. \. \# \# \. \. \# \. \# \# \. \# \. \. \. \# \#]
   [\. \# \# \. \. \# \. \# \. \. \# \# \. \. \# \# \. \# \. \. \# \# \. \. \#]
   [\. \# \# \. \. \# \# \# \# \# \. \. \. \. \# \# \# \# \# \. \# \. \# \. \#]
   [\# \. \. \# \. \. \# \. \. \# \# \. \. \. \# \. \. \# \. \# \. \# \. \# \#]])
