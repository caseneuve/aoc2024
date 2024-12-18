(ns tools
  (:require [clojure.string :refer [split-lines]]
            [clojure.test :refer [deftest testing is run-tests]]))

(defn file->str [dir]
  (slurp (str dir "/" "input.txt")))

(defn file->lines [dir]
  (split-lines (file->str dir)))

(defn str->ints [s]
  (->> s (re-seq #"-*\d+") (map parse-long)))

(defn transpose [coll]
  (apply mapv vector coll))

(defn manhattan [a b] (->> [a b] (apply map (comp abs -)) (apply +)))

(def dirs [[0 1] [-1 0] [0 -1] [1 0]])

(defn v+
  ([pos coll] (mapv #(mapv + pos %) coll))
  ([pos] (v+ pos dirs)))

(defn moves [pos] (v+ pos))

(comment
  (do
    (deftest str->ints-test
      (testing "transforms string into list of ints"
        (is (= '(1 3 2 4) (str->ints "1 asd 3 (QWER 2: 4)."))))
      (testing "is negative numbers aware"
        (is (= '(-1 3 -2 4) (str->ints "-1 3 -2 4"))))
      (testing "returns empty list when no ints found"
        (is (= () (str->ints "no ints")))))

    (deftest transform-test
      (testing "transforms rows into cols in matrix"
        (is (= (transpose ["ab" "cd" "ef"]) [[\a \c \e] [\b \d \f]]))))

    (deftest v+-moves-test
      (testing "returns vector of moves by directions"
        (is (= (v+ [0 0] dirs) [[0 1] [-1 0] [0 -1] [1 0]])))
      (testing "using `dirs` by default"
        (is (= (v+ [0 0]) [[0 1] [-1 0] [0 -1] [1 0]])))
      (testing "`moves` is default `v+`"
        (is (= (moves [0 0]) (v+ [0 0])))))

    (run-tests 'tools))
  )
