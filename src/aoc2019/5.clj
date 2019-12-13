(ns aoc2019.5
  (:require [clojure.edn :as edn]
            [aoc2019.utils :refer :all]))

(def ops
  {1 {:f ADD :length 3}
   2 {:f MULT :length 3}
   3 {:f READ :length 1}
   4 {:f WRITE :length 1}
   99 {:f EOF :length 0}})

(defn EOF [state] :EOF)

(defn READ
  [state out]
  (assoc state out (edn/read-string (read-line))))

(defn WRITE
  [state out]
  (do
    (println (get state out))
    state))

(defn ADD
  [state in1 in2 out]
  (assoc state out (+ (get state in1) (get state in2))))

(defn MULT
  [state in1 in2 out]
  (assoc state out (* (get state in1) (get state in2))))

(defn parse-program
  [s]
  (apply vector
    (map edn/read-string
         (clojure.string/split s #","))))

(defn execute-program
  [program]
  (loop [pointer 0
         state program]
    (let [opcode (get state pointer)
          {:keys [length f]} (get ops opcode)
          next-pointer-pos (+ pointer (inc length))
          next-state (apply (partial f state)
                            (take length (drop (inc pointer) state)))] ;; move pointer length + opcode
      (if (= :EOF next-state)
        state
        (recur next-pointer-pos next-state)))))

(defn run-program-string
  [s]
  (execute-program (parse-program s)))

(def prog (slurp "/Users/duncan/Dropbox/aoc2019/02input.txt"))
(def prog2 "1,0,0,0,99")

;; part 1
;; 3895705
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
