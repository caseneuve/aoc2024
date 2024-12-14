(ns day13.solution
  (:require [tools :refer [file->str]]))

(defn parse [s]
  (->> s (re-seq #"\d+") (map parse-long) (partition 6)))

(defn tokens [part [ax ay bx by X Y]]
  (let [[X Y] (cond->> [X Y] (= part 2) (mapv #(+ % 10000000000000)))
        M (/ (- (* Y ax) (* X ay)) (- (* by ax) (* bx ay)))
        N (/ (- X (* M bx)) ax)]
    (when (and (int? M) (int? N))
      (+ (* N 3) M))))

(defn solve [it part] (transduce (keep (partial tokens part)) + it))

(defn -main [day]
  (let [p (partial solve (parse (file->str day)))]
    {:part1 (p 1) :part2 (p 2)}))


(comment
  (let [test-input "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"
        [_first _second _third _fourth :as it] (parse test-input)]
    (assert (= 480 (solve it 1)))
    (assert (= (solve [_first]  2) 0))
    (assert (> (solve [_second] 2) 0))
    (assert (= (solve [_third] 2) 0))
    (assert (> (solve [_fourth] 2) 0)))
  )
