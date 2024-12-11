(ns day11.solution
  (:require [tools :refer [file->str]]))

(def blink
  (memoize
   (fn [pebble to-go]
     (cond (= 0 to-go) 1
           (= 0 pebble) (blink 1 (dec to-go))
           :else
           (let [s (str pebble), q (/ (count s) 2)]
             (if (int? q)
               (+ (blink (parse-long (subs s 0 q)) (dec to-go))
                  (blink (parse-long (subs s q)) (dec to-go)))
               (blink (* 2024 pebble) (dec to-go))))))))

(defn solve [s blinks]
  (transduce (map #(blink (parse-long %) blinks)) + (re-seq #"\d+" s)))

(defn -main [day]
  (let [blinks (partial solve (file->str day))]
    {:part1 (blinks 25) :part2 (blinks 75)}))


(comment
    (assert (= 55312 (solve "125 17" 25)))
  )
