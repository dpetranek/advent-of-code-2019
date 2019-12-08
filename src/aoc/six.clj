(ns aoc.six
  (:require [clojure.string :as str]))

(defn string->data [datastring]
  (->> (str/split-lines datastring)
       (map #(str/split % #"\)"))))


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

  (def a {"COM" {"B" {"G" {"H" "h"}
                      "C" {"D" {"I" "i"}
                           "E" {"F" "f"}
                           "J" {"K" {"L" "l"}}}}}})
  )


(defn build-paths [data]
  (reduce (fn [paths [anchor orbiter]]
            (let [parent-path (get paths anchor)]
              (assoc paths orbiter (concat parent-path [anchor]))))
          {}
          data))

(defn build-tree [paths]
  (reduce
   (fn [tree [key path]]
     (if-let [already-present? (get (get-in tree path) key)]
       tree
       (update-in tree (get paths key) #(assoc % key nil))))
   {}
   paths))

(comment
  (def paths (build-paths testdata))
  {"K" ("COM" "B" "C" "D" "E" "J"),
   "L" ("COM" "B" "C" "D" "E" "J" "K"),
   "G" ("COM" "B"),
   "J" ("COM" "B" "C" "D" "E"),
   "H" ("COM" "B" "G"),
   "E" ("COM" "B" "C" "D"),
   "C" ("COM" "B"),
   "F" ("COM" "B" "C" "D" "E"),
   "B" ("COM"),
   "I" ("COM" "B" "C" "D"),
   "D" ("COM" "B" "C")}

  (->> paths
       (map (fn [[k path]] (count path)))
       (reduce +))
  42


  (build-tree paths)
  {"COM" {"B" {"C" {"D" {"E" {"J" {"K" {"L" nil}},
                              "F" nil},
                         "I" nil}},
               "G" {"H" nil}}}}

  (->> (build-paths data)
       (map (fn [[k path]] (count path)))
       (reduce +))
  1913
  42
  1913

  )
