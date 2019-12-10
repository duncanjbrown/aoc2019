(ns aoc2019.2
  (:require [clojure.edn :as edn]
            [aoc2019.utils :refer :all]))

(defn compute-op
  [state [op in1 in2 out]]
  (let [operation (get {1 + 2 *} op)
        operands [(get state in1) (get state in2)]]
      (assoc state out (apply operation operands))))

(defn parse-program
  [s]
  (apply vector
    (map edn/read-string
         (clojure.string/split s #","))))

(defn execute-program
  [program]
  (loop [pointer 0
         state program]
      (if (= 99 (get state pointer))
        state
        (recur (+ pointer 4) (compute-op state (take 4 (drop pointer state)))))))

(defn run-program-string
  [s]
  (execute-program (parse-program s)))

(def prog (slurp "/Users/duncan/Dropbox/aoc2019/02input.txt"))

;; part 1
(first (run-program-string prog))

;; part 2
(let
    [desired 19690720
     input (parse-program prog)]
  (for [x (range 99)
        y (range 99)]
    (let [this-input (assoc input 1 x 2 y)]
      (when (= desired (first (execute-program this-input)))
        (println (+ y (* 100 x)))))))
