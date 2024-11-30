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

    (run-tests 'tools))
  )
