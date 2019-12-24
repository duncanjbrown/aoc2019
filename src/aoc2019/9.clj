(ns aoc2019.9
  (:require [clojure.edn :as edn]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan close! go-loop]]
            [aoc2019.utils :refer :all]))

(defn dereference-value
  [state relbase mode-number val-or-pointer]
  (let [mode (get [:position :immediate :relative] mode-number)]
    (cond (= :immediate mode) val-or-pointer
          (= :relative mode) (get state (+ relbase val-or-pointer))
          :else (get state val-or-pointer))))

(defn zero-pad
  "Zero-pads a vector v to the right up to a maximum size n"
  [v n]
  (if-let [padding (take (- n (count v)) (repeat 0))]
    (vec (reverse (apply conj (reverse v) padding)))))

(defn get-opcode-flags
  [opcode argcount]
  (zero-pad (drop 2 (reverse (digits opcode))) argcount))

(defn maybe-dereference-args
  [state relbase opcode args]
  (let [flags (get-opcode-flags opcode (count args))]
    (map (partial dereference-value state relbase) flags args)))

(defn READ
  [input state pointer relbase]
  (let [[opcode arg1] (take 2 (drop pointer state))
        [out-addr]    (maybe-dereference-args state relbase opcode [arg1])
        val (<!! input)]
    (println (str "READING: " val " TO: " out-addr))
    [(assoc state out-addr val) (+ pointer 2) relbase]))

(defn WRITE
  [output state pointer relbase]
  (let [[opcode arg1] (take 2 (drop pointer state))
        _ (println "WRITE")
        [value]       (maybe-dereference-args state relbase opcode [arg1])]
    (>!! output value)
    [state (+ pointer 2) relbase]))

(defn ADD
  [state pointer relbase]
  (let [[opcode arg1 arg2 out] (take 4 (drop pointer state))
         args (maybe-dereference-args state relbase opcode [arg1 arg2])]
    [(assoc state out (apply + args)) (+ pointer 4) relbase]))

(defn MULT
  [state pointer relbase]
  (let [[opcode arg1 arg2 out] (take 4 (drop pointer state))
        args (maybe-dereference-args state relbase opcode [arg1 arg2])]
    [(assoc state out (apply * args)) (+ pointer 4) relbase]))

(defn JUMPIF
  [state pointer relbase]
  (let [[opcode arg1 arg2] (take 3 (drop pointer state))
         args (maybe-dereference-args state relbase opcode [arg1 arg2])]
    (if-not (zero? (first args))
      [state (second args) relbase]
      [state (+ 3 pointer) relbase])))

(defn JUMPUNLESS
  [state pointer relbase]
  (let [[opcode arg1 arg2] (take 3 (drop pointer state))
         args (maybe-dereference-args state relbase opcode [arg1 arg2])]
    (if (zero? (first args))
      [state (second args) relbase]
      [state (+ 3 pointer) relbase])))

(defn LT
  [state pointer relbase]
  (let [[opcode arg1 arg2 out] (take 4 (drop pointer state))
         args (maybe-dereference-args state relbase opcode [arg1 arg2])]
    (if (apply < args)
      [(assoc state out 1) (+ 4 pointer) relbase]
      [(assoc state out 0) (+ 4 pointer) relbase])))

(defn EQ
  [state pointer relbase]
  (let [[opcode arg1 arg2 out] (take 4 (drop pointer state))
         args (maybe-dereference-args state relbase opcode [arg1 arg2])]
    (if (apply = args)
      [(assoc state out 1) (+ 4 pointer) relbase]
      [(assoc state out 0) (+ 4 pointer) relbase])))

(defn SETRELBASE
  [state pointer relbase]
  (let [[opcode arg1] (take 2 (drop pointer state))
         [change] (maybe-dereference-args state relbase opcode [arg1])]
    [state (+ 2 pointer) (+ relbase change)]))

(defn parse-program
  [s]
  (apply vector
    (map edn/read-string
      (clojure.string/split s #","))))

(defn execute-program
  [program output input]
  (let [ops {1 ADD
             2 MULT
             3 (partial READ input)
             4 (partial WRITE output)
             5 JUMPIF
             6 JUMPUNLESS
             7 LT
             8 EQ
             9 SETRELBASE}]
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
  (def input (slurp "day09.txt"))
  (run-program input [1]))
