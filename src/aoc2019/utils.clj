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
