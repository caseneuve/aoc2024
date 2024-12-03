(ns day03.solution
  (:require [tools :refer [file->str]]))

(defn solve [input part]
  (let [ops (re-seq #"mul\(\d+,\d+\)|do\(\)|don't\(\)" input)
        mul #(apply * (map read-string (re-seq #"\d+" %)))]
    (loop [[op & xs] ops, enabled true, sum 0]
      (if (nil? op) sum
          (condp = op
            "do()"    (recur xs true sum)
            "don't()" (recur xs false sum)
                      (recur xs enabled (cond-> sum (or enabled (= part 1)) (+ (mul op)))))))))

(defn -main [day]
  (let [p (partial solve (file->str day))]
    {:part1 (p 1) :part2 (p 2)}))


(comment
  (let [test-1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
        test-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"]
    (assert (= 161 (solve test-1 1)))
    (assert (=  48 (solve test-2 2))))
  )
