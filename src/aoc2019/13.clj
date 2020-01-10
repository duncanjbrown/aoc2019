(ns aoc2019.13
  (:require [aoc2019.11.computer :as computer]
            [clojure.core.async
             :as a
              :refer [>! <! >!! <!! go chan close! go-loop]]
            [quil.core :as q]))

(defn start-game
  [program i]
  (let [i (chan)
        o (chan)]
    (go (computer/run-program-string program o i))
    o))

(defn get-block
  [x y]
  [(* x 10) (* y 10) 10 10])

(def fill-colour
  {0 [0 0 0]
   1 [169 169 169]
   2 [3 252 3]
   3 [3 177 352]
   4 [252 3 3]})

(comment
  (def prog (slurp "day13.txt"))
  (def prog2 (slurp "day13-2.txt"))

  (def part1
    (let [output (start-game prog)]
        (loop [count 0])
        (let [x (<!! output)
                y (<!! output)
                t (<!! output)]
            (cond)
            (nil? x) (do (close! output) count)
            (= t 2) (recur (inc count))
            :else (recur count))))

  (let [input  (chan)
        output (start-game prog2 input)]

    (defn draw-frame
      []
      (println "frame")
      (dotimes [n (* 40 20)]
        (go (let [x (<!! output)
                  y (<!! output)
                  t (<!! output)]
              (if x
                (if (= -1 x)
                  (println t)
                  (do
                    (q/fill (fill-colour t))
                    (apply q/rect (get-block x y))))
                (do
                  (println "closing!")
                  (close! output))))))
      (println "done"))

    (defn setup
        []
        (go-loop [] (>! input 0))
        (q/frame-rate 1)
        (q/background 100 100 100))

    (q/defsketch example                ;; Define a new sketch named example
        :title "AOC 2019 Day 11.2"    ;; Set the title of the sketch
        :settings #(q/smooth 2)             ;; Turn on anti-aliasing
        :setup setup
        :draw draw-frame
        :size [400 300]
        :features [:keep-on-top])))
