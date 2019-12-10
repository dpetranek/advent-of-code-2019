(ns aoc.ten
  (:require [clojure.string :as str]))

(defn raw->data [raw]
  (mapv (comp vec seq) (str/split-lines raw)))

(defn slope [origin target]
  (let [[x1 y1] origin
        [x2 y2] target]
    (if (= x1 x2)
      [:vert (< y1 y2)]
      [(/ (- y2 y1) (- x2 x1)) (< x1 x2)])))

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


(defn count-asteroids [field vantage]
  (let [[x1 y1] vantage]
    (->> field
         (remove #{vantage})
         (map (fn [[x2 y2]]
                [(slope [x1 y1] [x2 y2])
                 (distance [x1 y1] [x2 y2])
                 [x2 y2]]))
         (group-by first)
         (keys)
         (count)
         #_(vals)
         #_(map (partial sort-by second))
         #_(map first)
         #_(count))))

(defn find-best-vantage [raw]
  (let [data      (raw->data raw)
        field     (assign-coordinates data)
        asteroids (filter-asteroids field)]
    (->> asteroids
         (map #(vector % (count-asteroids asteroids %)))
         (sort-by second)
         (last))))

(defn raw->asteroids [raw]
  (let [data      (raw->data raw)
        field     (assign-coordinates data)]
    (filter-asteroids field)))


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
  (def test3 "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.")
  (def test4 ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..")
  (def test5 ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")
  (def data1 (raw->data test1))
  (def coords1 (assign-coordinates data1))
  (def asteroids1 (mapcat (partial keep (fn [[x y a]] (when (= a \#) [x y]))) coords1))
  coords1
  asteroids1
  (def data2 (raw->data test2))
  (def coords2 (assign-coordinates data2))
  (def asteroids2 (mapcat (partial keep (fn [[x y a]] (when (= a \#) [x y]))) coords2))

  (let [field (assign-coordinates data1)
        asteroids (filter-asteroids field)]
    (->> asteroids
         (map #(vector % (count-asteroids asteroids %)))
         (sort-by second)
         (last)))

  (find-best-vantage test1)
  [[3 4] 8]
  ;; [[5 8] 33]
  (find-best-vantage test2)
  [[5 8] 33]
  [[0 1] 32]
  (raw->asteroids test2)
  [[0 1] 33]
  (find-best-vantage test3)
  [[1 2] 35]
  (find-best-vantage test4)
  [[6 3] 41]
  (find-best-vantage test5)
  [[11 13] 210]
  (find-best-vantage raw)
  [[11 19] 253]


  [[1 8] 38]
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
