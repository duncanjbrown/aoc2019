(ns aoc2019.8
  (:require [aoc2019.utils :refer :all]))

(def input (map parse-int
             (clojure.string/split
               (slurp "day08.txt")
               #"")))

(defn layers
  [encoded-image]
  (let [width  25
        height 6
        layer-size (* width height)]
    (partition layer-size input)))

(defn part1
  []
  (->> (layers input)
    (map frequencies)
    (sort-by #(get % 0))
    (first)
    ((juxt #(get % 1) #(get % 2)))
    (reduce *)))

(defn pixel-on-pixel
  [plo phi]
  (if (= phi 2) plo phi))

(defn layer-on-layer
  [l1 l2]
  (map pixel-on-pixel l1 l2))

(defn tr
  [a b x]
  (if (= x a)
    b
    x))

(defn print-line
  [line]
  (->> (map (partial tr 0 " ") line)))

(defn part2
  []
  (->> (layers input)
    (reverse)
    (reduce layer-on-layer)
    (partition 25)
    (map print-line)
    (map println)))
