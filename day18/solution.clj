(ns day18.solution
  (:require [tools :as t]
            [clojure.string :refer [join]]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse [s] (->> s t/str->ints (partition 2) (mapv vec)))

;; p. 1: dijstra
(defn shortest-path [bytes [X Y :as exit] cut]
  (let [BYT (set (take cut bytes))]
    (loop [Q (priority-map [[0 0] #{[0 0]}] 0), BEST Integer/MAX_VALUE, SCORE-MAP {}]
      (if (empty? Q) (dec BEST)
          (let [[[pos seen] _] (peek Q)]
            (if (= pos exit) (recur (pop Q), (min (count seen) BEST), SCORE-MAP)
                (let [[mvs scs]
                      (reduce
                       (fn [[mv bs] [x y :as pos]]
                         (let [s (count seen)]
                           (if (and (<= 0 x X) (<= 0 y Y)
                                    (not (contains? BYT pos)) (not (contains? seen pos))
                                    (< s (SCORE-MAP pos Integer/MAX_VALUE)))
                             [(conj mv [[pos (conj seen pos)] s]) (assoc bs pos s)]
                             [mv bs])))
                       [[] SCORE-MAP] (t/moves pos))]
                  (recur (into (pop Q) mvs), BEST, scs))))))))

;; p. 2: bfs
(defn locked? [bytes [X Y :as exit] cut]
  (let [BYT (set (take cut bytes))]
    (loop [Q (conj clojure.lang.PersistentQueue/EMPTY [0 0]), SEEN #{[0 0]}]
      (if (empty? Q) true
          (let [pos (peek Q)]
            (if (= pos exit) false
                (let [mvs (remove
                           (fn [[x y :as p]]
                             (or (contains? BYT p) (contains? SEEN p)
                                 (< x 0) (< y 0) (> y Y) (> x X)))
                           (t/moves pos))]
                  (recur (into (pop Q) mvs) (into SEEN mvs)))))))))

(defn first-blocking [bytes exit start]
  (loop [min_ (inc start), max_ (count bytes)]
    (if (= min_ max_) (join "," (bytes (dec min_)))
        (let [cut (+ min_ (quot (- max_ min_) 2))]
          (if (locked? bytes exit cut)
            (recur min_ cut)
            (recur (inc cut) max_))))))

(defn -main [day]
  (let [it (parse (t/file->str day))]
    {:part1 (shortest-path  it [70 70] 1024)
     :part2 (first-blocking it [70 70] 1024)}))


(comment
  (let [test-input
        (parse "5,4 4,2 4,5 3,0 2,1 6,3 2,4 1,5 0,6 3,3 2,6 5,1 1,2 5,5 2,5 6,5 1,4 0,4 6,4 1,1 6,1 1,0 0,5 1,6 2,0")]
    (assert (= 22 (shortest-path test-input [6 6] 12)))
    (assert (= "6,1" (first-blocking test-input [6 6] 12))))
  )
