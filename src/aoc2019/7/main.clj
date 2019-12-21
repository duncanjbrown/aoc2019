(ns aoc2019.7.main
  (:require [aoc2019.7.computer :as computer]
            [clojure.math.combinatorics :refer [permutations]]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan close! go-loop]]))

(def amp-program (slurp "day07.txt"))

(defn start-amp
  [output initial-setting]
  (let [input (computer/run-program-string amp-program output)]
    (>!! input initial-setting)
    input))

(defn compute-output-signal
  [a b c d e]
  (let [output-channel (chan)]
    (-> (start-amp output-channel e)
        (start-amp d)
        (start-amp c)
        (start-amp b)
        (start-amp a)
      (>!! 0))
    (<!! output-channel)))

(defn permute
  []
  (let [numbers [0 1 2 3 4]]
    (apply max (map #(apply compute-output-signal %) (permutations numbers)))))

(permute)
