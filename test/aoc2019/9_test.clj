(ns aoc2019.9-test
  (:require [aoc2019.9 :as sut]
            [clojure.core.async :refer [<!! >! chan go-loop close!]]
            [clojure.test :refer :all]))

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
    (sut/run-program-string program output input)
    (<!! output)))

(deftest intcode-day-5
  (testing "position mode"
    (let [prog "3,9,8,9,10,9,4,9,99,-1,8"]
        (is (= (run-program prog [8]) 1))
        (is (zero? (run-program prog [7])))))
  (testing "immediate mode"
    (let [prog "3,3,1108,-1,8,3,4,3,99"]
        (is (= (run-program prog [8]) 1))
        (is (zero? (run-program prog [7])))))
  (testing "jumping"
    (let [prog "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"]
      (is (zero? (run-program prog [0])))
      (is (= (run-program prog [999]) 1)))))

(deftest relative-mode
  (testing "relative base is 0"
    (is (zero? (sut/dereference-value [0 1 2 3 4] 0 2 0))))
  (testing "relative base is 1"
    (is (= 1 (sut/dereference-value [0 1 2 3 4] 1 2 0))))
  (testing "relative base is -1"
    (is (= 3 (sut/dereference-value [0 1 2 3 4] -1 2 4)))))

(deftest set-relative-mode
  (let [prog "9,1,204,0,99"]
    (is (= 10 (run-program prog [])))))
