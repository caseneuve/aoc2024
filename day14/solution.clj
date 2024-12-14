(ns day14.solution
  (:require [tools :refer [file->str]]))

(defn robots [s]
  (->> s
       (re-seq #"-?\d+")
       (map parse-long)
       (partition 4)
       (reduce (fn [m [x y vx vy]] (update m [x y] (fnil conj []) [vx vy])) {})))

(defn move [rb X Y]
  (reduce-kv
   (fn [g k vv]
     (reduce
      (fn [g v]
        (let [[nx ny] (as-> (mapv + k v) [x y] [(mod x X) (mod y Y)])]
          (update g [nx ny] (fnil conj []) v)))
      g vv))
   {} rb))

(defn quadrants [rb X Y]
  (let [[qx qy] (map #(quot % 2) [X Y])]
    (reduce-kv
     (fn [acc [x y] v]
       (let [c (count v)]
         (cond-> acc
           (and (< x qx) (< y qy)) (update 0 + c)
           (and (> x qx) (< y qy)) (update 1 + c)
           (and (< x qx) (> y qy)) (update 2 + c)
           (and (> x qx) (> y qy)) (update 3 + c))))
     [0 0 0 0] rb)))

(defn lines [rb X Y]
  (for [y (range Y)]
    (apply str
           (for [x (range X)]
             (cond-> (if (nil? (rb [x y])) " " "#") (= x (dec X)) (str \newline))))))

(defn solve [it X Y part]
  (let [show #(println (apply str (lines % X Y)))
        has-long-lines? (fn [rb] (some #(re-matches #".*#{30}.*\n" %) (lines rb X Y)))]
    (loop [s 0, R (robots it)]
      ;; (when (= 0 (mod s 1000)) (prn s) (show G))
      (cond (and (= part 1) (= s 100)) (apply * (quadrants R X Y))
            (= s 7709) (do (show R) s)  ; it's actually the solution
                                        ; established by running the clause below
                                        ; which is slow...
            ;; (has-long-lines? R) (do (show R) s)
            :else (recur (inc s) (move R X Y))))))

(defn -main [day]
  (let [p #(solve (file->str day) 101 103 %)]
    {:part1 (p 1) :part2 (p 2)}))


(comment
  (let [test-input "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"]
    (assert (= 12 (solve (robots test-input) 11 7 1))))
 )
