(ns aoc2019.10)

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs ^Integer (- x2 x1)) (Math/abs ^Integer (- y2 y1))))

(defn slope
  [v]
  (if (= 0 (second v))
    :undefined
    (apply / v)))

(defn find-visible
  [candidates origin]
  (let [vectors (map #(map - %1 %2) (filter (complement #{origin}) candidates) (repeat origin))
        intersecting (vals (group-by slope vectors))
        spokes (mapcat (fn [x] (partition-by #(map neg? %) x)) intersecting)]
    (map #(sort-by (partial manhattan-distance origin) %) spokes)))

(filter (complement #{1}) [ 1 2 3])

(find-visible [[0 0] [2 0] [0 4] [2 4] [4 8]] [1 2])

(defn parse-input
  [in]
  (loop [x 0
         y 0
         input in
         output []]
    (let [[curr & rst] input]
      (cond
        (= curr \.)
        (recur (inc x) y rst output)
        (= curr \#)
        (recur (inc x) y rst (conj output [x y]))
        (= curr \newline)
        (recur 0 (inc y) rst output)
        :else
        output))))

(let [puzzle (parse-input (slurp "day10.txt"))]
  (->> (map #(count (find-visible puzzle %)) puzzle)
    sort
    last))

(defn skimmer
  [colls]
  (lazy-seq
    (let [ss (keep seq colls)]
      (when (seq ss)
        (cons (map first ss) (skimmer (map rest ss)))))))

; part 1: [22 25]
(apply count (take 1 (skimmer vvs)))
(comment
    (let [puzzle (parse-input (slurp "day10.txt"))]
        (->> (zipmap puzzle (map (comp first #(find-visible puzzle %)) puzzle))
            (sort-by (comp count second))
            last))

    (def vvs (let [puzzle (parse-input (slurp "day10.txt"))]
               (->> (find-visible puzzle [22 25])))))
