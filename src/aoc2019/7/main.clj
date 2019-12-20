(ns aoc2019.7.main
  (:require [aoc2019.7.computer :as computer]))

(comment
    (def prog (slurp "05input.txt"))
    (let [input (computer/run-program-string prog)]
        (>!! input 1)))
