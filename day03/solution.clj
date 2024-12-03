(ns day03.solution
  (:require [tools :refer [file->str]]
            [clojure.string :as str]))

(defn clear-donts [s] (str/replace s #"(?s)don't\(\).*?(do\(\)|\Z)" ""))

(defn solve [input part]
  (let [ops (re-seq #"mul\(\d+,\d+\)" (cond-> input (= part 2) clear-donts))
        mul #(apply * (map read-string (re-seq #"\d+" %)))]
    (->> ops (map mul) (apply +))))

(defn -main [day]
  (let [p (partial solve (file->str day))]
    {:part1 (p 1) :part2 (p 2)}))


(comment
  (let [test-1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
        test-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"]
    (assert (= 161 (solve test-1 1)))
    (assert (=  48 (solve test-2 2))))
  )
