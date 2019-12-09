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
  ("COM" "7VN" "2VC" "L4P" "3VZ" "2DB" "BPW" "TSP" "H7L" "6NX" "LP5" "QSH" "P3Z" "XS5" "RLY" "LNP" "MKP" "927" "LD4" "J5Z" "TTK" "WXX" "8J9" "ZPG" "JBR" "TYR" "159" "B8L" "PND" "QYX" "718" "JK9" "GTJ" "FTC" "TKK" "729" "153" "YMN" "SCZ" "GMM" "BHX" "9H6" "XBT" "92T" "6QK" "DSL" "ZGT" "B7T" "YW7" "GTH" "K43" "DSP" "T43" "JFQ" "NG7" "VG4" "64H" "X6N" "4RR" "3ZN" "99C" "LST" "7VD" "GXK" "RXL" "2RY" "DR4" "MVN" "1NL" "9ZB" "8GT" "C9X" "9XC" "BS4" "RQR" "2Y3" "1KG" "TKJ" "JWT" "G86" "K8Q" "XHQ" "KGX" "1SW" "GXF" "NM8" "4HT" "783" "5WX" "71P" "NR1" "TDX" "51M" "QZZ" "RBP" "Z86" "3VK" "J26" "L1Q" "1WM" "6JV" "DJS" "PGP" "VX4" "H83" "PHV" "2WM" "FYP" "GLZ" "YKY" "4P5" "9DN" "RXQ" "5N8" "58B" "CNL" "7QK" "8XR" "DFQ" "Y7W" "D7D" "SFK" "6SM" "BZX" "Y7S")
  (get (build-paths data) "YOU")
  ("COM" "7VN" "2VC" "L4P" "3VZ" "2DB" "BPW" "TSP" "H7L" "6NX" "LP5" "QSH" "P3Z" "XS5" "RLY" "LNP" "MKP" "927" "LD4" "J5Z" "TTK" "WXX" "8J9" "ZPG" "JBR" "TYR" "159" "B8L" "PND" "QYX" "718" "JK9" "GTJ" "FTC" "TKK" "729" "153" "YMN" "SCZ" "GMM" "BHX" "9H6" "XBT" "92T" "6QK" "DSL" "ZGT" "B7T" "YW7" "GTH" "K43" "DSP" "T43" "JFQ" "NG7" "VG4" "64H" "X6N" "4RR" "3ZN" "99C" "LST" "7VD" "C97" "1PW" "DJH" "NYX" "F3N" "Q6K" "CN3" "HKD" "DXF" "R8G" "LP9" "7B7" "VXT" "65J" "FNB" "1DB" "QLX" "ZF8" "681" "QJN" "BB4" "RPY" "PSL" "3P8" "GZQ" "BKC" "RNR" "81K" "X1S" "QCT" "BYH" "27N" "WWX" "F84" "WGN" "TD3" "3T3" "T2L" "YF5" "139" "9QT" "27D" "PQH" "8Y6" "JRY" "5DL" "9H5" "94G" "C81" "W1L" "71V" "WSP" "8XH" "SCQ" "Z75" "NMM" "RK2" "KPF" "V72" "YYQ" "519" "Q4L" "J2S" "JNX" "MP3" "SM2" "P49" "4VS" "R92" "LXF" "HVY" "2XK" "HDM" "4RV" "HMD" "RX9" "MJP" "MZV" "17Y" "BMG" "MRV" "7J6" "6RB" "LDP" "R5Z" "X5Y" "9LV" "P95" "42J" "TWR" "YF3" "VW9" "1HX" "LQV" "8Q8" "VF2" "PVS" "BJY" "NWT" "VB7" "SZL" "ZHB" "66B" "BRZ" "HQ9" "HFZ" "GRD" "YRN" "D53" "TZP" "YS8" "PC5" "LYP" "8LL" "T2Q" "727" "H4Q" "DV9" "J5D" "9WC" "GMN" "CD9" "H1X" "Q8J" "5S9" "V5N" "JHQ" "KLX" "JPJ" "J9R" "RR7" "V6L" "LQX" "JNR" "DJF" "RZD" "N67" "1TZ" "YWS" "V17" "34F" "12V" "X1X" "WST" "137" "ZQB" "99Z" "Z7R" "JG9" "B41" "C8Z" "1LP" "BW4" "JXK" "4N1" "W3P" "HHY" "WKD" "RP2" "4FP" "2L2" "ZCB" "6HJ" "B63" "MYQ" "NDY" "QFS" "7TP" "WM2" "S1H" "QDP" "9WW" "DZY" "RPR" "L2T" "F8L" "TXG" "TMQ" "FK4" "38B" "X8T" "FP7" "1H7" "7X8" "YQC" "5W3" "K2T" "MK5" "5SY" "LDK" "VBD" "7TX" "XNG" "NYW" "GHQ" "XPZ" "RQZ")




  (interleave (range 3) (range 6))
  (0 0 1 1 2 2)


  nil
  nil

  testlookup
  (build-paths-once-and-for-all testdata)
  42




  (build-paths-once-and-for-all data)
  144909

  (count (set/union (set/difference (set (get (build-paths data) "YOU"))
                                    (set (get (build-paths data) "SAN")))
                    (set/difference (set (get (build-paths data) "SAN"))
                                    (set (get (build-paths data) "YOU")))))
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
