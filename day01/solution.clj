(ns day01.solution
  (:require [tools :refer [file->str]]))

(defn parse [it]
  (->> it
       (re-seq #"\d+")
       (map read-string)
       (partition 2)
       (apply map list)))

(defn part1 [lists]
  (reduce + (apply mapv (comp abs -) (map sort lists))))

(defn part2 [[left right]]
  (let [fqs (frequencies right)]
    (reduce (fn [s n] (+ (* n (fqs n 0)) s)) 0 left)))

(defn -main [day]
  (let [lists (parse (file->str day))]
    {:part1 (part1 lists) :part2 (part2 lists)}))


(comment
  (let [test-input "3   4
4   3
2   5
1   3
3   9
3   3"
        input (->> test-input parse)]
    (assert (= 11 (part1 input)))
    (assert (= 31 (part2 input))))
  )
