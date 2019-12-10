(ns aoc.six
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn string->data [datastring]
  (->> (str/split-lines datastring)
       (map #(str/split % #"\)"))
       (reduce (fn [res [orbitee orbiter]] (assoc res orbiter orbitee)) {})))


(comment
  (def raw (slurp "resources/6.txt"))

  (def data (string->data raw))

  (def test "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")

  (def testdata (string->data test))

  )

#_(defn build-tree [paths]
  (reduce
   (fn [tree [key path]]
     (if-let [already-present? (get (get-in tree path) key)]
       tree
       (update-in tree (get paths key) #(assoc % key nil))))
   {}
   paths))

(defn build-path [lookup orbiter]
  (loop [orbiter orbiter
         path []]
    (if-let [parent (get lookup orbiter)]
      (recur parent (conj path parent))
      path)))

(defn build-paths [lookup]
  (reduce (fn [paths orbiter]
            (assoc paths orbiter (build-path lookup orbiter)))
          {}
          (keys lookup)))

(comment
  (build-path testdata "H")
  ["G" "B" "COM"]
  ("COM" "B" "G")
  ["G" "B" "COM"]
  ["G" "B" "COM"]
  ["K" "J" "E" "D" "C" "B" "COM"]
  ["C" "B" "COM"]

  (build-paths testdata)
  {"J" ("COM" "B" "C" "D" "E"), "K" ("COM" "B" "C" "D" "E" "J"), "B" ("COM"), "E" ("COM" "B" "C" "D"), "G" ("COM" "B"), "D" ("COM" "B" "C"), "COM" (), "C" ("COM" "B")}
  )

(defn build-paths-once-and-for-all [lookup]
  (->> (map (partial build-path lookup) (keys lookup))
       (map count)
       (reduce +)))

(defn find-lowest-common-ancestor [path1 path2]
  )

(comment
  (def paths (build-paths data))
  (count data)
  1107

  (get (build-paths data) "SAN")

  (get (build-paths data) "YOU")


  (interleave (range 3) (range 6))
  (0 0 1 1 2 2)


  nil
  nil

  testlookup
  (build-paths-once-and-for-all testdata)
  42




  (build-paths-once-and-for-all data)
  144909

  (let [you (set (get (build-paths data) "YOU"))
        san (set (get (build-paths data) "SAN"))]
    (count (set/union (set/difference you san)
                      (set/difference san you))))
  259

  ;; or
  (let [you (set (get (build-paths data) "YOU"))
        san (set (get (build-paths data) "SAN"))]
    (count (set/difference (set/union you san)
                           (set/intersection you san))))
  259


  259
  197
  322)
