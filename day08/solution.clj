(ns day08.solution
  (:require [tools :refer [file->str]]
            [clojure.string :refer [trim]]
            [clojure.math.combinatorics :as c]))

(defn grid [s]
  (reduce
   (fn [[antennas [y x]] ch]
     (if (= ch \newline)
       [antennas [(inc y) 0]]
       [(cond-> antennas (not= ch \.) (update ch (fnil conj []) [y x]))
        [y (inc x)]]))
   [{} [0 0]] s))

(defn solve [s]
  (let [[ANT [Y X]] (->> s trim grid)
        pairs (->> ANT vals (mapcat #(c/combinations % 2)))
        in? (fn [[y x]] (and (<= 0 y Y) (<= 0 x (dec X))))
        v+ #(mapv + %1 %2), v- #(mapv - %1 %2)
        p1 (fn [[a b]] (filter in? [(v+ a (v- a b)), (v+ b (v- b a))]))
        p2 (fn [[a b]]
             (letfn [(f [p d] (take-while in? (iterate #(v+ % d) p)))]
               (concat (f a (v- a b)) (f b (v- b a)))))]
    (map #(->> pairs (mapcat %) set count) [p1 p2])))

(defn -main [day]
  (zipmap [:part1 :part2] (solve (file->str day))))


(comment
  (let [test-input "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"]
    (assert (= [14 34] (solve test-input))))
  )
