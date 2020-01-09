(ns aoc2019.12
  (:require [aoc2019.utils :refer :all]))

(def example-moons [{:id 1 :pos [-1 0 2] :vel [0 0 0]}
                    {:id 2 :pos [2 -10 -7] :vel [0 0 0]}
                    {:id 3 :pos [4 -8 8] :vel [0 0 0]}
                    {:id 4 :pos [3 5 -1] :vel [0 0 0]}])

(def example-moons2 [{:id 1 :pos [-8 -10 0] :vel [0 0 0]}
                     {:id 2 :pos [5 5 10] :vel [0 0 0]}
                     {:id 3 :pos [2 -7 3] :vel [0 0 0]}
                     {:id 4 :pos [9 -8 -3] :vel [0 0 0]}])

(def moons [{:id 1 :pos [-2 9 -5] :vel [0 0 0]}
            {:id 2 :pos [16 19 9] :vel [0 0 0]}
            {:id 3 :pos [0 3 6] :vel [0 0 0]}
            {:id 4 :pos [11 0 11] :vel [0 0 0]}])

(defn resolve-plane-gravity
  [linep1 linep2]
  (cond
      (< linep1 linep2) [1 -1]
      (> linep1 linep2) [-1 1]
      :else [0 0]))

(defn resolve-gravity
  "Given two positions, obtain their gravity vectors"
  [pos1 pos2]
  (take 2 (skim (map resolve-plane-gravity pos1 pos2))))

(defn update-with-gravity
  [moon gravity]
  (let [new-vel (map + (:vel moon) gravity)]
    (-> moon
        (assoc :vel new-vel)
        (update :pos #(map + %1 %2) new-vel))))

(defn tick
  [moons]
  (let [null-gravity-delta (zipmap (map :id moons) (repeat [0 0 0]))
        pairs (for [m1 moons
                    m2 moons
                    :when (< (:id m1) (:id m2))]
                [m2 m2])
        gravity-deltas (reduce (fn [deltas [m1 m2]]
                                 (let [[new-grav-1 new-grav-2] (apply resolve-gravity (map :pos [m1 m2]))]
                                   (-> deltas
                                       (update (:id m1) #(map + %1 %2) new-grav-1)
                                       (update (:id m2) #(map + %1 %2) new-grav-2))))
                         null-gravity-delta pairs)]
    (map (fn [moon]
           (update-with-gravity moon (get gravity-deltas (:id moon)))) moons)))

(defn calculate-energy
  [moon]
  (* (reduce + (map #(. Math abs %) (:pos moon)))
     (reduce + (map #(. Math abs %) (:vel moon)))))

(def system (iterate tick moons))
(def example (iterate tick example-moons))

(defn slice-moon-by-axis
  [moon axis-n]
  (-> moon
      (update :pos #(vector (get % axis-n)))
      (update :vel #(vector (get % axis-n)))))

(defn find-period
  [system]
  (let [starting-moon (first system)]
    (first
      (keep-indexed
        (fn [idx turn]
            (when (and (pos? idx) (= starting-moon turn))
              idx)) system))))

;; tyvm https://rosettacode.org/wiki/Least_common_multiple#Clojure
(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(comment
    ;; part 2
    (let [moons-x (iterate tick (map #(slice-moon-by-axis % 0) moons))
          moons-y (iterate tick (map #(slice-moon-by-axis % 1) moons))
          moons-z (iterate tick (map #(slice-moon-by-axis % 2) moons))]
        (reduce lcm (map find-period [moons-x moons-y moons-z])))

    (def part1 (reduce + (map calculate-energy (nth system 1000))))

    part1)
