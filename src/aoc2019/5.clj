(ns aoc2019.5
  (:require [clojure.edn :as edn]
            [aoc2019.utils :refer :all]))

(defn READ
  [state pointer]
  (let [[_ out] (take 2 (drop pointer state))]
    [(assoc state out (edn/read-string (read-line))) (+ pointer 2)]))

(defn PRINT
  [state pointer]
  (let [[opcode arg1] (take 2 (drop pointer state))
        value         (first (maybe-dereference-args state opcode [arg1]))]
    (do
        (println value)
        [state (+ pointer 2)])))

(defn ADD
  [state pointer]
  (let [[opcode arg1 arg2 out] (take 4 (drop pointer state))
        args (maybe-dereference-args state opcode [arg1 arg2])]
    [(assoc state out (apply + args)) (+ pointer 4)]))

(defn MULT
  [state pointer]
  (let [[opcode arg1 arg2 out] (take 4 (drop pointer state))
        args (maybe-dereference-args state opcode [arg1 arg2])]
    [(assoc state out (apply * args)) (+ pointer 4)]))

(defn JUMPIF
  [state pointer]
  (let [[opcode arg1 arg2] (take 3 (drop pointer state))
        args (maybe-dereference-args state opcode [arg1 arg2])]
    (if-not (zero? (first args))
      [state (second args)]
      [state (+ 3 pointer)])))

(defn JUMPUNLESS
  [state pointer]
  (let [[opcode arg1 arg2] (take 3 (drop pointer state))
        args (maybe-dereference-args state opcode [arg1 arg2])]
    (if (zero? (first args))
      [state (second args)]
      [state (+ 3 pointer)])))

(defn LT
  [state pointer]
  (let [[opcode arg1 arg2 out] (take 4 (drop pointer state))
        args (maybe-dereference-args state opcode [arg1 arg2])]
    (if (apply < args)
      [(assoc state out 1) (+ 4 pointer)]
      [(assoc state out 0) (+ 4 pointer)])))

(defn EQ
  [state pointer]
  (let [[opcode arg1 arg2 out] (take 4 (drop pointer state))
        args (maybe-dereference-args state opcode [arg1 arg2])]
    (if (apply = args)
      [(assoc state out 1) (+ 4 pointer)]
      [(assoc state out 0) (+ 4 pointer)])))


(defn parse-program
  [s]
  (apply vector
    (map edn/read-string
         (clojure.string/split s #","))))

(defn maybe-dereference-args
  [state opcode args]
  (let [flags (get-opcode-flags opcode (count args))]
    (map (partial dereference-value state) flags args)))

(defn dereference-value
  [state mode-number val-or-pointer]
  (let [mode (get [:position :immediate] mode-number)]
    (if (= :immediate mode)
      val-or-pointer
      (get state val-or-pointer))))

(defn get-opcode-flags
  [opcode argcount]
  (reverse (zero-pad (drop 2 (reverse (digits opcode))) argcount)))

(defn load-opcode
  [opcode]
  (let [ops {1 ADD
             2 MULT
             3 READ
             4 PRINT
             5 JUMPIF
             6 JUMPUNLESS
             7 LT
             8 EQ}]
    (get ops (mod opcode 100))))

(defn zero-pad
  "Zero-pads a vector v to the right up to a maximum size n"
  [v n]
  (if-let [padding (take (- n (count v)) (repeat 0))]
    (apply conj (reverse v) padding)))

(defn execute-program
  [program]
  (loop [state program
         pointer 0]
    (if (= 99 (get state pointer))
      state
      (let [opcode (load-opcode (get state pointer))
            [new-state new-pointer] (opcode state pointer)]
          (recur new-state new-pointer)))))

(defn run-program-string
  [s]
  (execute-program (parse-program s)))

(def prog (slurp "05input.txt"))

(run-program-string prog)

;; (run-program-string "3,9,8,9,10,9,4,9,99,-1,8")
;; (run-program-string "3,9,7,9,10,9,4,9,99,-1,8")
