(ns aoc2019.6
  (:require [clojure.edn :as edn]
            [aoc2019.utils :refer :all]))

(defn parse-input
  []
  (->> (read-file "6.txt")
       (map #(clojure.string/split % #"\)"))
       (map reverse)))

(defn input->adj-list
  [input]
  (reduce (fn [acc [a b]]
              (assoc acc a b)) {} input))

(defn build-path
  ([paths from]
   (build-path paths from "COM"))
  ([paths from to]
   (let [next-orbit (get paths from)]
     (if (= to next-orbit)
       (seq [to])
       (conj (build-path paths next-orbit to) next-orbit)))))

(defn part1
  [adj-list]
  (let [nodes (keys adj-list)]
    (reduce (fn [acc nxt]
              (+ acc (count (build-path adj-list nxt)))) 0 nodes)))

(defn part2
  [adj-list]
  (let [you-to-com (build-path adj-list "YOU")
        san-to-com (build-path adj-list "SAN")
        origin (first you-to-com)
        target (first san-to-com)
        isec (first (drop-while (fn [x] (not-any? #(= x %) san-to-com)) you-to-com))]
   (reduce + (map count [(build-path adj-list origin isec) (build-path adj-list target isec)]))))

(comment
    (def adj-list (-> (parse-input) input->adj-list))
    (part1 adj-list)
    (part2 adj-list))
