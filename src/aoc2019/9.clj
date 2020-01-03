(ns aoc2019.9
  (:require [clojure.edn :as edn]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan close! go-loop]]
            [aoc2019.utils :refer :all]))

(defn zero-pad
  "Zero-pads a vector v to the right up to a maximum size n"
  [v n]
  (if-let [padding (take (- n (count v)) (repeat 0))]
    (vec (reverse (apply conj (reverse v) padding)))))

(defn get-opcode-flags
  [opcode argcount]
  (zero-pad (drop 2 (reverse (digits opcode))) argcount))

(defn prepare-args
  [opcode args argtypes state relbase]
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
       (get-opcode-flags opcode (count args))))

(defn io-read
  [input state pointer relbase]
  (let [argtypes [:write]
        [opcode & args] (take 2 (drop pointer state))
        [out] (prepare-args opcode args argtypes state relbase)
        val (<!! input)]
    [(assoc state out val) (+ pointer 2) relbase]))

(defn io-write
  [output state pointer relbase]
  (let [argtypes [:read]
        [opcode & args] (take 2 (drop pointer state))
        [arg1] (prepare-args opcode args argtypes state relbase)]
    (>!! output arg1)
    [state (+ pointer 2) relbase]))

(defn calculate
  [f]
  (fn
    [state pointer relbase]
    (let [argtypes [:read :read :write]
          [opcode & args] (take 4 (drop pointer state))
          [arg1 arg2 out] (prepare-args opcode args argtypes state relbase)]
        [(assoc state out (f arg1 arg2)) (+ pointer 4) relbase])))

(defn jump
  [pred]
  (fn
    [state pointer relbase]
    (let [argtypes [:read :read]
          [opcode & args] (take 3 (drop pointer state))
          [arg1 arg2] (prepare-args opcode args argtypes state relbase)]
      (if (pred arg1)
        [state arg2 relbase]
        [state (+ 3 pointer) relbase]))))

(defn cmp
  [pred]
  (fn
    [state pointer relbase]
    (let [argtypes [:read :read :write]
          [opcode & args] (take 4 (drop pointer state))
          [arg1 arg2 out] (prepare-args opcode args argtypes state relbase)]
      (if (pred arg1 arg2)
        [(assoc state out 1) (+ 4 pointer) relbase]
        [(assoc state out 0) (+ 4 pointer) relbase]))))

(defn setrelbase
  [state pointer relbase]
  (let [argtypes [:read]
        [opcode & args] (take 2 (drop pointer state))
        [arg1] (prepare-args opcode args argtypes state relbase)]
    [state (+ 2 pointer) (+ relbase arg1)]))

(defn parse-program
  [s]
  (apply vector
    (map edn/read-string
      (clojure.string/split s #","))))

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
