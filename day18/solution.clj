(ns day18.solution
  (:require [tools :as t]
            [clojure.string :refer [join]]))

(defn parse [s] (->> s t/str->ints (partition 2) (mapv vec)))

;; solves both parts (dijkstra is faster for the p. 1, see README)
(defn bfs [bytes [X Y :as exit] cut]
  (let [bytes (set (take cut bytes))
        nxt (fn [pos len seen]
              (reduce
               (fn [[nb vis :as acc] [x y :as p]]
                 (if (and (not (contains? bytes p)) (not (contains? seen p)) (<= 0 x X) (<= 0 y Y))
                   [(conj nb [p (inc len)]) (conj vis p)]
                   acc)) [[] seen]
               (t/moves pos)))]
    (loop [Q (conj clojure.lang.PersistentQueue/EMPTY [[0 0] 0]), seen #{[0 0]}]
      (if (empty? Q) nil
          (let [[pos path] (peek Q)]
            (if (= pos exit) path
                (as-> (nxt pos path seen) [m s] (recur (into (pop Q) m) s))))))))

;; bisect
(defn first-blocking [bytes exit start]
  (loop [min_ (inc start), max_ (count bytes)]
    (if (= min_ max_) (join "," (bytes (dec min_)))
        (let [cut (+ min_ (quot (- max_ min_) 2))]
          (if (nil? (bfs bytes exit cut))
            (recur min_ cut)
            (recur (inc cut) max_))))))

(defn -main [day]
  (let [it (parse (t/file->str day))]
    {:part1 (bfs it [70 70] 1024)
     :part2 (first-blocking it [70 70] 1024)}))


(comment
  (let [test-input
        (parse "5,4 4,2 4,5 3,0 2,1 6,3 2,4 1,5 0,6 3,3 2,6 5,1 1,2 5,5 2,5 6,5 1,4 0,4 6,4 1,1 6,1 1,0 0,5 1,6 2,0")]
    (assert (= 22 (bfs test-input [6 6] 12)))
    (assert (= "6,1" (first-blocking test-input [6 6] 12))))
  )
