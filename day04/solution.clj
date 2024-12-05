(ns day04.solution
  (:require [tools :refer [file->lines file->str]]
            [clojure.string :refer [split-lines]]))

(defn -main [day]
  (let [input (file->lines day)]
    {:part1 nil :part2 nil}))

(defn diag [starts mx X Y fx fy]
  (loop [[pp & rr] starts, lines []]
    (if (empty? pp) lines
        (recur rr
               (conj lines
                     (loop [lx (first pp), ly (second pp), line []]
                       (if (or (< lx 0) (= lx X) (< ly 0) (= ly Y)) line
                           (recur (fx lx) (fy ly) (conj line ((mx ly) lx))))))))))


;; (defn diag* [mx]
;;   (let [Y (count mx), X (count (first mx))]
;;     (reduce
;;      (fn [m line] (conj m (mapv (fn [[x y]] (get (mx y) x \.)) line)))
;;      []
;;      (concat
;;       (map #(mapv vector (range) (range % -1 -1)) (range 0 Y))
;;       (map #(mapv vector (range % X) (range (dec Y) -1 -1)) (range 1 X))))))


(comment
  (let [test-input "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"
        ;;         test-input "..X...
        ;; .SAMX.
        ;; .A..A.
        ;; XMAS.S
        ;; .X...."
        test-input (file->str "day04")
        input (->> test-input split-lines (mapv vec))
        mx input
        X (count (first mx))
        Y (count mx)
        U (for [x (range X)] [x 0])
        L (for [y (range Y)] [0 y])
        D (for [x (range X)] [x (dec Y)])
        SE (into #{} (concat L U))
        NE (into #{} (concat L D))
        ]

    ;; p1
    [
     (->>
      (concat
       (map #(apply str %) mx)
       (map #(apply str %) (map reverse mx))

       (map #(apply str %) (apply mapv vector mx))
       (map #(apply str %) (map reverse (apply mapv vector mx)))

       (map #(apply str %) (diag SE mx X Y inc inc))
       (map #(apply str %) (map reverse (diag SE mx X Y inc inc)))

       (map #(apply str %) (diag NE mx X Y inc dec))
       (map #(apply str %) (map reverse (diag NE mx X Y inc dec))))

      (map (fn [s] (re-seq #"XMAS" s)))
      (remove nil?)
      (map count)
      (apply +))

     ;; p2
     (loop [x 0, y 0, count 0]
       (if (= y (- Y 2)) count
           (let [SQ (mapv vec (for [yy (range y (+ 3 y))] (take 3 (drop x (mx yy)))))
                 [nx ny] (if (= x (- X 3)) [0 (inc y)] [(inc x) y])
                 ;; use diag here?
                 x-mas? (and
                         (= \A ((SQ 1) 1))
                         (or
                          (and (= \M ((SQ 0) 0)) (= \S ((SQ 2) 2)))
                          (and (= \S ((SQ 0) 0)) (= \M ((SQ 2) 2))))
                         (or
                          (and (= \M ((SQ 2) 0)) (= \S ((SQ 0) 2)))
                          (and (= \S ((SQ 2) 0)) (= \M ((SQ 0) 2)))))]
             (recur nx ny (cond-> count x-mas? inc)))))]
    )
  [2593 1950]
  )
