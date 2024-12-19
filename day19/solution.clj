(ns day19.solution
  (:require [tools :refer [file->str]]
            [clojure.string :refer [split-lines starts-with?]]))

(defn parse [it]
  (let [[stripes _ & patterns] (split-lines it)]
    [(re-seq #"\w+" stripes) patterns]))

(def matches
  (memoize
   (fn [design stripes]
     (if (empty? design) 1
         (transduce
          (keep
           (fn [s] (when (starts-with? design s) (matches (subs design (count s)) stripes))))
          + stripes)))))

(defn -main [day]
  (let [[stripes designs] (parse (file->str day))]
    {:part1 (count (filter #(> (matches % stripes) 0) designs))
     :part2 (transduce (map #(matches % stripes)) + designs)}))


(comment
  (let [[s p] (->  "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb" parse)]
    (assert (= 6 (count (filter #(> (matches % s) 0) p))))
    (assert (= 16 (transduce (map #(matches % s)) + p))))
 )
