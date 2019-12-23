(ns aoc2019.7.main
  (:require [aoc2019.7.computer :as computer]
            [clojure.math.combinatorics :refer [permutations]]
            [clojure.core.async
             :as a
             :refer [<! >! >!! <!! chan alts! alts!! go-loop close!]]))

(def amp-program (slurp "day07.txt"))

(defn start-amp
  ([output initial-setting]
   (start-amp output initial-setting (chan)))
  ([output initial-setting input]
   (let [in (computer/run-program-string amp-program output input)]
     (>!! in initial-setting)
     input)))

(defn splitter
  [amp-a thrusters]
  (let [input (chan)]
    (go-loop []
      (if-let [x (<! input)]
        (if (>! amp-a x)
          (recur)
          (>! thrusters x))
        (close! input)))
    input))

(defn compute-output-signal
  [a b c d e]
  (let [thrusters (chan)
        input-a   (chan)]
    (-> (splitter input-a thrusters)
        (start-amp e)
        (start-amp d)
        (start-amp c)
        (start-amp b)
        (start-amp a input-a)
      (>!! 0))
    (<!! thrusters)))

(defn permute
  []
  (let [numbers [5 6 7 8 9]]
    (apply max (map #(apply compute-output-signal %) (permutations numbers)))))

(permute)
