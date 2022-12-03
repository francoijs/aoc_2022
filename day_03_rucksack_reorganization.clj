(ns day-03
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-input "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")
(def rucksacks (str/split-lines test-input))

;; return index of char i in string s, or a negative number
(defn search-item [i s]
  (java.util.Collections/binarySearch (sort s) i))
(is (= 5 (search-item \p "vJrwpWtwJgWr")))
(is (= 10 (search-item \p "hcsFMMfFFhFp")))
(is (> 0 (search-item \p "hcsFMMfFFhF")))

;; return first char found in 2 or 3 strings
(defn find-common-item
  ([s1 s2] (reduce (fn [ret c]
                     (if (>= (search-item c s2) 0)
                       (reduced c)
                       ret))
                   -1
                   s1))
  ([s1 s2 s3] (reduce (fn [ret c]
                        (if (and (>= (search-item c s2) 0)
                                 (>= (search-item c s3) 0))
                          (reduced c)
                          ret))
                      -1
                      s1)))
(is (= \p (find-common-item "vJrwpWtwJgWr" "hcsFMMfFFhFp")))
(is (> 0 (find-common-item "vJrwpWtwJgWr" "hcsFMMfFFhF")))
(is (= \r (apply find-common-item (take 3 rucksacks))))
(is (= \Z (apply find-common-item (take-last 3 rucksacks))))

;; return char that is found both in 1st and 2nd halves of a string
(defn find-duplicate-item [s]
  (let [half-len (/ (count s) 2)]
    (find-common-item (take half-len s)
                      (drop half-len s))))                      
(is (= \p (find-duplicate-item (first rucksacks))))
(is (= -1 (find-duplicate-item "123456")))
(is (= \0 (find-duplicate-item "012034")))

(defn priority [c]
  (apply - (map int (if (< (int c) 97)
                      (list c \A -27)
                      (list c \a -1)))))
(is (= 1 (priority \a)))
(is (= 52 (priority \Z)))

;; part 1
(is (= 8252 (->> (slurp "day_03_rucksack_reorganization.txt")
                 str/split-lines
                 (map find-duplicate-item)
                 (map priority)
                 (apply +))))

;; part 2
(is (= 2828 (->> (slurp "day_03_rucksack_reorganization.txt")
                 str/split-lines
                 (partition 3)
                 (map (fn [l] (apply find-common-item l)))
                 (map priority)
                 (apply +))))
