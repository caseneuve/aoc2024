(ns day10.solution
  (:require [tools :refer [file->str]]))

(defn grid [input]
  (reduce
   (fn [[g sp [y x]] ch]
     (if (= ch \newline) [g sp [(inc y) 0]]
         [(assoc g [y x] (read-string (str ch)))
          (cond-> sp (= ch \0) (conj [y x]))
          [y (inc x)]]))
   [(sorted-map) [] [0 0]] input))

(def dirs [[-1 0] [0 -1] [1 0] [0 1]])

(defn hike [G from]
  (letfn [(mv [p] (filter #(= 1 (- (G % 0) (G p))) (map #(mapv + p %) dirs)))]
    (loop [Q (into clojure.lang.PersistentQueue/EMPTY (map #(list % from) (mv from))), TH {}]
      (if (empty? Q) TH
          (let [[pos prev & _ :as path] (peek Q)]
            (if (= 9 (G pos))
              (recur (pop Q) (update TH pos (fnil inc 0)))
              (recur (into (pop Q) (map #(conj path %) (->> (mv pos) (filter #(not= prev %))))), TH)))))))

(defn solve [it]
  (let [[G S _] (grid it), th (map #(hike G %) S)]
    {:part1 (->> th (mapcat keys) count)
     :part2 (->> th (mapcat vals) (apply +))}))

(defn -main [day] (->> day file->str solve))


(comment
  (let [test-input "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"]
    (assert (= {:part1 36 :part2 81} (solve test-input))))
  )
