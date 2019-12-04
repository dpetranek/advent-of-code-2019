(ns aoc.four
  (:require [clojure.set :as set]))


(def pass-range [206938 679128])

(defn match-n? [num-adjacent]
  (fn [n]
    (->> (seq (str n))
         (partition num-adjacent 1)
         (filter (partial apply =)))))

(defn increasing? [n]
  (let [digits (seq (str n))]
    (= digits (sort digits))))

(def match-2? (match-n? 2))
(def match-3? (match-n? 3))

(defn find-digit-groups [size n]
  (->> (seq (str n))
       (partition size 1)
       (filter (partial apply =))
       (mapcat identity)
       (set)))

(defn digit-repeated-twice-but-not-more? [n]
  (let [repeated3 (find-digit-groups 3 n)
        repeated2 (find-digit-groups 2 n)]
    (not-empty (remove repeated3 repeated2))))

(comment
  (match-2? 206687)
  true
  true
  '((\6 \6))
  false
  '((\6 \6))
  '((\2 \0) (\0 \6) (\6 \6) (\6 \8) (\8 \7))
  '(\2 \0 \6 \6 \8 \7)

  (match-2? 206587)
  false

  (match-3? 206667)
  true
  (match-3? 206666)
  true
  (match-3? 206632)
  false

  (let [n 766665]
    (and (match-2? n)
         (not (match-3? n))))
  false
  (let [n 766543]
    (and (match-2? n)
         (not (match-3? n))))
  true
  (let [n 766555
        match-2s (->> (seq (str n))
                      (partition 2 1))
        match-3s (->> (seq (str n))
                      (partition 3 1))
        repeated3 (->> match-3s
                       (filter (partial apply =))
                       (mapcat identity)
                       (set))
        repeated2 (->> match-2s
                       (filter (partial apply =))
                       (mapcat identity)
                       (set))]
    (remove repeated3 repeated2))
  (\6)
  #{\5 \6}
  #{\5}
  #{\5 \6}
  (\6 \6 \5 \5 \5 \5)
  ((\6 \6) (\5 \5) (\5 \5))
  ((\7 \6) (\6 \6) (\6 \5) (\5 \5) (\5 \5))

  #{\5}
  #{(\5 \5 \5)}
  ((\5 \5 \5))
  ((\7 \6 \6) (\6 \6 \5) (\6 \5 \5) (\5 \5 \5))
  ((\7 \6 \6) (\6 \6 \5) (\6 \5 \5))

  ((\6 \6) (\6 \6) (\6 \6) (\5 \5))
  ((\7 \6) (\6 \6) (\6 \6) (\6 \6) (\6 \5) (\5 \6) (\6 \5) (\5 \5))
  (\7 \6 \6 \6 \6 \5 \6 \5 \5)
  ((\7 \6 \6) (\6 \6 \5) (\6 \5 \5))
  ((\5 \5 \5))
  [((\7 \6) (\6 \6) (\6 \5) (\5 \5) (\5 \5)) ((\7 \6 \6) (\6 \6 \5) (\6 \5 \5) (\5 \5 \5))]
  [((\6 \6) (\5 \5) (\5 \5)) ((\5 \5 \5))]
  [true true]
  false

  (digit-repeated-twice-but-not-more? 222444)
  nil
  ()
  (\2)

  (->> (apply range pass-range)
       (filter (fn [n] (and (increasing? n) (digit-repeated-twice-but-not-more? n))))
       (count))
  1133
  1681
  1681
  755
  56
  1653)
