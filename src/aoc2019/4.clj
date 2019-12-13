(ns aoc2019.4
  (:require [clojure.edn :as edn]
            [clojure.test :as t]
            [aoc2019.utils :refer :all]))

;; https://stackoverflow.com/a/29929434/751089
(defn digits [n]
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(defn p1-acceptable-password?
  [n]
  (let [dgts (digits n)]
    (and (= (count dgts) 6)
         (< (count (partition-by identity dgts)) 6) ;; at least one adjacent pair
         (apply <= dgts))))

(defn p2-acceptable-password?
  [n]
  (let [dgts (digits n)
        groups (partition-by identity dgts)]
    (and (= (count dgts) 6)
         (< (count groups) 6)
         (some #(= (count %) 2) groups)
         (apply <= dgts))))

(count (filter p1-acceptable-password? (range 248345 746315)))
(count (filter p2-acceptable-password? (range 248345 746315)))
