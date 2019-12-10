(ns aoc2019.1
  (:require [clojure.edn :as edn]
            [aoc2019.utils :refer :all]))

(defn fuel-required
  [mass]
  (-> mass
      (quot 3)
      (- 2)))

(->> (read-file "/Users/duncan/Dropbox/aoc2019/0101input")
     (map edn/read-string)
     (map fuel-required)
     (reduce +))

(->> (read-file "/Users/duncan/Dropbox/aoc2019/0101input")
     (map edn/read-string)
     (map fuel-required-recursive)
     (reduce +))

(defn fuel-required-recursive
  [mass]
  (let [next-fuel (fuel-required mass)]
    (if (<= next-fuel 0)
      0
      (+ next-fuel (fuel-required-recursive next-fuel)))))

(fuel-required-recursive 100756)
