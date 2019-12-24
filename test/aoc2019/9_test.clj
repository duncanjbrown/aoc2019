(ns aoc2019.9-test
  (:require [aoc2019.9 :as sut]
            [clojure.test :refer :all]))

(deftest intcode-day-5
  (testing "position mode"
    (let [prog "3,9,8,9,10,9,4,9,99,-1,8"]
        (is (= (sut/run-program prog [8]) [1]))
        (is (= [0] (sut/run-program prog [7])))))
  (testing "immediate mode"
    (let [prog "3,3,1108,-1,8,3,4,3,99"]
        (is (= [1] (sut/run-program prog [8])))
        (is (= [0] (sut/run-program prog [7])))))
  (testing "jumping"
    (let [prog "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"]
      (is (= [0] (sut/run-program prog [0])))
      (is (= [1] (sut/run-program prog [999]))))))

(deftest relative-mode
  (testing "relative base is 0"
    (is (zero? (sut/dereference-value [0 1 2 3 4] 0 2 0))))
  (testing "relative base is 1"
    (is (= 1 (sut/dereference-value [0 1 2 3 4] 1 2 0))))
  (testing "relative base is -1"
    (is (= 3 (sut/dereference-value [0 1 2 3 4] -1 2 4)))))

(deftest set-relative-mode
  (let [prog "9,1,204,0,99"]
    (is (= [1] (sut/run-program prog [])))))

(deftest worked-examples
  ;; (testing "quine"
  ;;   (let [prog "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"]
  ;;     (is (= [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99]
  ;;            (sut/run-program prog [])))))
  ;; (testing "calculate a big number"
  ;;   (let [prog "1102,34915192,34915192,7,4,7,99,0"]
  ;;     (is (= [1219070632396864]
  ;;            (sut/run-program prog [])))))
  ;; (testing "print a big number"
  ;;   (let [prog "104,1125899906842624,99"]
  ;;     (is (= [1125899906842624]
  ;;            (sut/run-program prog [])))))
  ;; (testing "help!"
    ;; (let [prog "109,-1,204,1,99"]
    ;;   (is (= [109]
    ;;          (sut/run-program prog []))))
    ;; (let [prog "109,1,9,2,204,-6,99"]
    ;;   (is (= [204]
    ;;          (sut/run-program prog []))))
    ;; (let [prog "109,1,109,9,204,-6,99"]
    ;;   (is (= [204]
    ;;          (sut/run-program prog []))))
    ;; (let [prog "109,1,209,-1,204,-106,99"]
    ;;   (is (= [204]
    ;;          (sut/run-program prog []))))
    ;; (let [prog "109,1,3,3,204,2,99"]
    ;;   (is (= [99]
    ;;          (sut/run-program prog [99]))))
    (let [prog "109,1,203,2,204,2,99"]
      (is (= [99]
            (sut/run-program prog [99])))))
