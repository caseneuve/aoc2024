(ns day02.solution
  (:require [tools :refer [file->str]]
            [clojure.string :refer [split-lines]]))

(defn parse [s]
    (->> s split-lines (map (fn [l] (map read-string (re-seq #"\d+" l))))))

(defn same-vector-change [coll]
  (every? #(= (neg? (first coll)) (neg? %)) (rest coll)))

(defn safe-step-change [coll]
  (every? #(<= 1 (abs %) 3) coll))

(defn list-gen [coll]
  (for [n (range (count coll))]
    (concat (take n coll) (drop (inc n) coll))))

(defn solve [input part]
  (let [pairs (fn [c] (map #(partition 2 1 %) c))
        diffs (fn [c] (map #(map (fn [p] (apply - p)) %) c))
        f (if (= part 1) (fn [f & args] (apply f args)) map)
        it (cond->> input (= part 2) (map list-gen))]
    (->> it
         (f pairs)
         (f diffs)
         (f #(filter same-vector-change %))
         (f #(filter safe-step-change %))
         (remove empty?)
         count)))

(defn -main [day]
  (let [input (parse (file->str day))]
    {:part1 (solve input 1) :part2 (solve input 2)}))


(comment
  (let [test-input (-> "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"
                    parse)]
    (assert (= 2 (solve test-input 1)))
    (assert (= 4 (solve test-input 2))))
  )
