(ns day05.solution
  (:require [tools :refer [file->str]]
            [clojure.string :refer [split-lines split]]))

(defn s->ints [s]
  (->> s split-lines (map #(re-seq #"\d+" %)) (mapv #(mapv parse-long %))))

(defn solve [s]
  (let [[ORD PAG] (map s->ints (split s #"\n\n"))
        RUL (reduce (fn [m [a b]] (update m a (fnil conj #{}) b)) {} ORD)
        COM (comparator #(not (contains? (RUL %2) %1)))]
    (->> PAG
         (map #(as-> (vec (sort COM %)) sx, [(= sx %) (sx (quot (count sx) 2))]))
         (reduce (fn [acc [ok n]] (update acc (if ok :part1 :part2) (fnil + 0) n)) {}))))

(defn -main [day] (solve (file->str day)))


(comment
  (let [test-input "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"]
    (assert (= {:part1 143, :part2 123} (solve test-input))))
  )
