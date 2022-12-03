(ns day-01
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-str "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defn sorted-calories [s]
  (as-> s %
    (str/split % #"\n")
    (map (fn [s] (case s "" 0 (Integer/parseUnsignedInt s))) %)
    (partition-using (fn [c] (zero? (first c))) %)
    (map (fn [v] (apply + v)) %)
    (sort > %)))

;; part 1
(is (= 24000 (first (sorted-calories test-str))))
(is (= 64929 (first (sorted-calories (slurp "day_01_calorie_counting.txt")))))

;; part 2
(is (= 45000 (apply + (take 3 (sorted-calories test-str)))))
(is (= 193697 (apply + (take 3 (sorted-calories (slurp "day_01_calorie_counting.txt"))))))
