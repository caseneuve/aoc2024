(ns day07.solution
  (:require [tools :refer [file->str]]
            [clojure.string :refer [split-lines]]
            [clojure.math.combinatorics :as c]))

(defn parse [it]
  (->> it split-lines (map #(map parse-long (re-seq #"\d+" %)))))

(defn solve [it part]
  (let [ops (cond-> [* +] (= part 2) (conj #(parse-long (str %1 %2))))
        f (fn [[res n & nx]]
            (some
             #(when (= res (reduce-kv (fn [acc k v] (if (> acc res) (reduced 0) ((% k) acc v))) n (vec nx)))
                res)
             (map vec (c/selections ops (count nx)))))]
    (->> it (pmap f) (filter int?) (apply +))))

(defn -main [day]
  (let [p (partial solve  (parse (file->str day)))]
    {:part1 (p 1) :part2 (p 2)}))


(comment

  (let [test-input (->> "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20" parse)]
    (assert (=  3749 (solve test-input 1)))
    (assert (= 11387 (solve test-input 2))))

  )
