(ns day23.solution
  (:require [tools :refer [file->str]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :refer [combinations]]))

(defn parse [input]
  (->> input
       (re-seq #"\w+")
       (partition 2)
       (reduce (fn [acc [a b]]
                 (-> acc
                     (update a (fnil conj #{}) b)
                     (update b (fnil conj #{}) a)))
               {})))

(defn part1 [G]
  (count
   (reduce-kv
    (fn [acc k v]
      (into acc
            (for [[a b] (combinations v 2)
                  :when (contains? (G a) b)
                  :when (contains? (G b) a)
                  :when (some #(str/starts-with? % "t") [k a b])]
              #{k a b})))
    #{} G)))

(defn bron-kerbosch
  ([G] (bron-kerbosch G #{} (keys G) #{}))
  ([G r p x]
   (if (and (empty? p) (empty? x)) [r]
       (loop [[v & rp :as p] p, x x, res []]
         (if (empty? p) res
             (let [cliques (bron-kerbosch G
                                          (conj r v)
                                          (set/intersection (set p) (G v))
                                          (set/intersection (set x) (G v)))]
               (recur rp, (conj x v), (into res cliques))))))))

(defn part2 [G]
  (->> (bron-kerbosch G) (apply max-key count) sort (str/join ",")))

(defn -main [day]
  (let [input (parse (file->str day))]
    {:part1 (part1 input) :part2 (part2 input)}))


(comment
  (let [test-input (-> "kh-tc qp-kh de-cg ka-co yn-aq qp-ub cg-tb vc-aq tb-ka wh-tc
yn-cg kh-ub ta-co de-co tc-td tb-wq wh-td ta-ka td-qp aq-cg wq-ub ub-vc de-ta
wq-aq wq-vc wh-yn ka-de kh-ta co-tc wh-qp tb-vc td-yn" parse)]
    (assert (= 7 (part1 test-input)))
    (assert (= "co,de,ka,ta" (part2 test-input))))
    )
