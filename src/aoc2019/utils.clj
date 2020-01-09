(ns aoc2019.utils
  (:require [clojure.edn :as edn]))

(defn parse-int
  [int-string]
  (edn/read-string int-string))

(defn read-file
  [path]
  (clojure.string/split-lines (slurp path)))

(def select-values (comp vals select-keys))

(defn take-upto
  "Returns a lazy sequence of successive items from coll up to and including
  the first item for which `(pred item)` returns true."
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result x]
        (let [result (rf result x)]
          (if (pred x)
            (ensure-reduced result)
            result))))))
  ([pred coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [x (first s)]
        (cons x (if-not (pred x) (take-upto pred (rest s)))))))))

;; https://stackoverflow.com/a/29929434/751089
(defn digits [n]
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(defn zero-pad
  "Zero-pads a vector v to the right up to a maximum size n"
  [v n]
  (if-let [padding (take (- n (count v)) (repeat 0))]
    (vec (reverse (apply conj (reverse v) padding)))))

; https://stackoverflow.com/a/40788757
(defn skim
  "Given a seq of seqs, return a lazy seq of the first element from each,
  the second element from each, etc"
  [colls]
  (lazy-seq
    (let [ss (keep seq colls)]
      (when (seq ss)
        (cons (map first ss) (skim (map rest ss)))))))
