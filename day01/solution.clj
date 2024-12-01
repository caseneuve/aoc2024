(ns day01.solution
  (:require [tools :refer [file->str]]))

(defn parse [it]
  (->> it
       (re-seq #"\d+")
       (map read-string)
       (partition 2)
       (apply map list)))

(defn part1 [lists]
  (->> lists
       (map sort)
       (apply map list)
       (reduce (fn [s p] (+ s (abs (apply - p)))) 0)))

(defn part2 [left right]
  (let [occurences (->> right (reduce (fn [m e] (update m e (fnil inc 0))) {}))]
    (->> left (reduce (fn [s n] (+ (* n (get occurences n 0)) s)) 0))))

(defn -main [day]
  (let [[L R :as lists] (parse (file->str day))]
    {:part1 (part1 lists) :part2 (part2 L R)}))


(comment
  (let [test-input "3   4
4   3
2   5
1   3
3   9
3   3"
        [L R :as input] (->> test-input parse)]
    (assert (= 11 (part1 input)))
    (assert (= 31 (part2 L R)))
    )
  )
