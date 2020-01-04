(ns aoc2019.9
  (:require [clojure.edn :as edn]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan close! go-loop]]
            [aoc2019.utils :refer :all]))

(defn prepare-args
  [argtypes state pointer relbase]
  (let [read-n (+ 1 (count argtypes))
        [opcode & args] (take read-n (drop pointer state))
        modes (zero-pad (drop 2 (reverse (digits opcode))) (count argtypes))]
    (map (fn [arg type mode-number]
            (let [mode (get [:position :immediate :relative] mode-number)]
                (if (= :read type)
                    (cond
                        (= :immediate mode) arg
                        (= :relative mode) (get state (+ relbase arg))
                        (= :position mode) (get state arg))
                    (cond
                        (= :relative mode) (+ relbase arg)
                        (= :position mode) arg))))
        args
        argtypes
        modes)))

(defn io-read
  [input state pointer relbase]
  (let [[out] (prepare-args [:write] state pointer relbase)]
    [(assoc state out (<!! input)) (+ pointer 2) relbase]))

(defn io-write
  [output state pointer relbase]
  (let [[arg1] (prepare-args [:read] state pointer relbase)]
    (>!! output arg1)
    [state (+ pointer 2) relbase]))

(defn calculate
  [f]
  (fn
    [state pointer relbase]
    (let [[arg1 arg2 out] (prepare-args [:read :read :write] state pointer relbase)]
        [(assoc state out (f arg1 arg2)) (+ pointer 4) relbase])))

(defn jump
  [pred]
  (fn
    [state pointer relbase]
    (let [[arg1 arg2] (prepare-args [:read :read] state pointer relbase)]
      (if (pred arg1)
        [state arg2 relbase]
        [state (+ 3 pointer) relbase]))))

(defn cmp
  [pred]
  (fn
    [state pointer relbase]
    (let [[arg1 arg2 out] (prepare-args [:read :read :write] state pointer relbase)]
      (if (pred arg1 arg2)
        [(assoc state out 1) (+ 4 pointer) relbase]
        [(assoc state out 0) (+ 4 pointer) relbase]))))

(defn setrelbase
  [state pointer relbase]
  (let [[arg1] (prepare-args [:read] state pointer relbase)]
    [state (+ 2 pointer) (+ relbase arg1)]))

(defn execute-program
  [program output input]
  (let [ops {1 (calculate +)
             2 (calculate *)
             3 (partial io-read input)
             4 (partial io-write output)
             5 (jump (complement zero?))
             6 (jump zero?)
             7 (cmp <)
             8 (cmp =)
             9 setrelbase}]
    (go-loop [state (zero-pad program 2048)
              pointer 0
              relbase 0]
      (if (= 99 (get state pointer))
        (do
          (close! input)
          (close! output)
          state)
        (let [opcode (get ops (mod (get state pointer) 100))
              [new-state new-pointer new-relbase] (opcode state pointer relbase)]
          (recur new-state new-pointer new-relbase))))
    input))

(defn parse-program
  [s]
  (apply vector
    (map edn/read-string
      (clojure.string/split s #","))))

(defn run-program-string
  [s output input]
  (execute-program (parse-program s) output input))

(defn run-program
  [program inputs]
  (let [input (chan)
        output (chan)]
    (go-loop [remaining-inputs inputs]
      (if-let [next-input (first remaining-inputs)]
        (do
            (>! input next-input)
            (recur (drop 1 inputs)))
        (close! input)))
    (run-program-string program output input)
    (<!! (a/into [] output))))

(comment
  (def prog (slurp "day09.txt"))
  (run-program prog [1])
  (run-program prog [2]))
