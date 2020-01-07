(ns aoc2019.11.core
  (:require [aoc2019.11.computer :as computer]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan close! go-loop]]))

(defn paint
  [painted pos colour]
  (if (zero? colour)
    (-> painted
      (update :black conj pos)
      (update :white disj pos))
    (-> painted
      (update :white conj pos)
      (update :black disj pos))))

; (paint {:black #{[0 0]} :white #{[1 1]}} [1 1] 0)
; (paint {:black #{[0 0]} :white #{[1 1]}} [0 0] 1)
; (paint {:black #{[0 0]} :white #{[1 1]}} [2 2] 1)

(defn get-colour
  [pos painted]
  (cond
    (some #{pos} (:black painted)) 0
    (some #{pos} (:white painted)) 1
    :else 0))

(defn next-direction
  [current-direction instruction]
  (let [directions (cycle [:n :e :s :w])
        current-index (.indexOf directions current-direction)]
    (if (zero? instruction)
      (nth directions (+ current-index 3))
      (nth directions (+ current-index 1)))))

(defn send-colour
  [colour input output]
  (let [out-chan (a/take 2 output)]
    (go (>! input colour))
    (let [result (<!! (a/into [] out-chan))]
      (if (seq result)
        result
        false))))

(defn tick
  [pos painted direction input output]
  (let [directions {:n [0 1] :e [1 0] :w [-1 0] :s [0 -1]}]
    (if-let [[new-colour turn-instruction] (send-colour (get-colour pos painted) input output)]
      (let [next-dir (next-direction direction turn-instruction)]
        (recur
            (map + pos (next-dir directions))
            (paint painted pos new-colour)
            next-dir
            input output))
      painted)))

(defn start-robot
  [program painted]
  (let [i (chan)
        o (chan)]
    (go (computer/run-program-string program o i))
    (tick [0 0] {:black #{} :white #{}} :n i o)))

(comment
  (def prog (slurp "day11.txt"))
  (def p1 (start-robot prog {:black #{} :white #{}}))

  (+ (count (:black p1))
    (count (:white p1)))

  (def p2 (start-robot prog {:black #{} :white #{[0 0]}})))


;2278 too low
