(ns aoc2019.10)

;; parsing
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

;; part 1
(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs ^Integer (- x2 x1)) (Math/abs ^Integer (- y2 y1))))

(defn manhattan-sort
  "Sort points by distance from the origin"
  [a b]
  (compare (manhattan-distance [0 0] a) (manhattan-distance [0 0] b)))

(defn slope
  [v]
  (if (zero? (second v))
    :undefined
    (apply / v)))

(defn find-visible
  [candidates origin]
  (let [vectors (map #(map - %1 %2) (remove #{origin} candidates) (repeat origin))
        intersecting (vals (group-by slope vectors))
        spokes (mapcat (fn [x] (partition-by #(map neg? %) x)) intersecting)]
    (map #(sort manhattan-sort %) spokes)))

(comment
    (let [puzzle (parse-input (slurp "day10.txt"))]
        (count (->> (map #(find-visible puzzle %) puzzle)
                    (sort-by count)
                    last))))

;; part 2

; https://stackoverflow.com/a/40788757
(defn skimmer
  "Given a seq of seqs, return a lazy seq of the first element from each,
  the second element from each, etc"
  [colls]
  (lazy-seq
    (let [ss (keep seq colls)]
      (when (seq ss)
        (cons (map first ss) (skimmer (map rest ss)))))))

;; https://stackoverflow.com/a/6989383
(defn clockwise-sort
  [[ax ay] [bx by]]
  (cond
    (and (>= ax 0) (neg? bx)) -1
    (and (neg? ax) (>= bx 0)) 1
    (and (= 0 ax bx) (>= ay 0) (>= by 0)) (compare ay by)
    (= 0 ax bx) (compare by ay)
    :else
    (let [loffset (- (* ax by) (* bx ay))]
      (cond
        (neg? loffset) -1
        (pos? loffset) 1
        :else (compare (+ (* ax ax) (* ay ay))
                 (+ (* bx bx) (* by by)))))))

(comment
  ;; get the original best position
    (let [puzzle (parse-input (slurp "day10.txt"))]
        (->> (map #(vector % (find-visible puzzle %)) puzzle)
             (sort-by (comp count second))
             last
             first))

    (def vvs (let [puzzle (parse-input (slurp "day10.txt"))]
               (find-visible puzzle [22 25])))

    (map +
        (->> (sort clockwise-sort (first (skimmer vvs)))
            ;; split into quadrants
            (partition-by #(map neg? %))
            ;; turn them upside-down as the laser starts pointing 'up' :/
            ((fn [[NE SE SW NW]] (apply concat (map reverse [SE NE NW SW]))))
            (take 200)
            (last))
        [22 25]))
