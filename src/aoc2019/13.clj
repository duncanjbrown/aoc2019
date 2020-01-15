(ns aoc2019.13
  (:require [aoc2019.11.computer :as computer]
            [clojure.core.async
             :as a
              :refer [>! <! >!! <!! alts!! go chan close! go-loop]]
            [quil.core :as q]))

(defn start-game
  [program i]
  (let [o (chan)]
    (go (computer/run-program-string program o i))
    o))

(defn get-block
  [x y]
  [(* x 20) (* y 20) 20 20])

(def fill-colour
  {0 [0 0 0]
   1 [169 169 169]
   2 [3 252 3]
   3 [3 177 352]
   4 [252 3 3]})


(def framebuffer (atom (vec (repeat 40 (vec (repeat 20 0))))))

(defn game-tick
  [input output]
  (go-loop []
      (let [[outcome port] (alts! [[input 0]]
                             (<! (a/into [] (a/take 3 output))))]
        (if (= output port)
          (let [[x y z] outcome]
            (swap! framebuffer assoc-in [x y] z)))
        (recur))))

(defn setup-framebuffer
  [output]
  (dotimes [n (* 40 20)]
    (println n)
    (let [[x y z] (<!! output)]
        (swap! framebuffer assoc-in [x y] z))))

(def prog (slurp "day13.txt"))
(def prog2 (slurp "day13-2.txt"))

(comment
  (def part1
    (let [output (start-game prog)]
        (loop [count 0]
          (let [x (<!! output)
                y (<!! output)
                t (<!! output)]
            (cond
              (nil? x) (do (close! output) count)
              (= t 2) (recur (inc count))
              :else (recur count))))))

  (let [input  (chan 20)
        output (start-game prog2 input)]

    (def framebuffer (atom (vec (repeat 40 (vec (repeat 20 0))))))

    (defn setup-framebuffer
        [prog-output-chan]
        (dotimes [n (* 40 20)]
            (let [[x y z] (seq (<!! (a/into [] (a/take 3 prog-output-chan))))]
                (swap! framebuffer assoc-in [x y] z))))

    (defn draw
      []
      (doall
        (for [x (range 40)
              y (range 20)
              :let [colour (fill-colour (get-in @framebuffer [x y]))]]
          (do
            (q/fill colour)
            (apply q/rect (get-block x y))))))

    (defn setup
      []
      (q/frame-rate 24)
      (q/background 100 100 100)
      (setup-framebuffer output))

    (q/defsketch example                ;; Define a new sketch named example
        :title "AOC 2019 Day 11.2"    ;; Set the title of the sketch
        :settings #(q/smooth 2)             ;; Turn on anti-aliasing
        :setup setup
        :draw draw
        :size [800 600]
        :features [:keep-on-top])))
