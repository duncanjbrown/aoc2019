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

(->> (layers input)
  (map frequencies)
  (sort-by #(get % 0))
  (first)
  ((juxt #(get % 1) #(get % 2)))
  (reduce *))
