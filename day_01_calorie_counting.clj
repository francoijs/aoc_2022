(ns day-01
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-input "1000
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
  (->> s
       str/split-lines
       (map #(case % "" 0 (parse-long %)))
       (partition-using (comp zero? first))
       (map #(apply + %))
       (sort >)))

;; part 1
(is (= 24000 (first (sorted-calories test-input))))
(is (= 64929 (first (sorted-calories (slurp "day_01_calorie_counting.txt")))))

;; part 2
(is (= 45000 (apply + (take 3 (sorted-calories test-input)))))
(is (= 193697 (apply + (take 3 (sorted-calories (slurp "day_01_calorie_counting.txt"))))))
