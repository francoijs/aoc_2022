(ns day-04
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-pairs "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn read-ranges [s]
  (let [v (map parse-long (str/split s #"[,-]"))]
    (partition 2 v)))

(defn fully-contains? [p1 p2]
  (and (>= (first p2) (first p1))
       (<= (second p2) (second p1))))

(defn fully-contained? [p1 p2]
  (or (fully-contains? p1 p2)
      (fully-contains? p2 p1)))

(assert (fully-contained? '(2 8) '(3 7)))
(assert (fully-contained? '(6 6) '(4 6)))
(assert (not (fully-contained? '(3 6) '(5 7))))

;; part 1
(assert (= 2 (->> test-pairs
                  str/split-lines
                  (map read-ranges)
                  (filter #(apply fully-contained? %))
                  count)))
(assert (= 550 (->> (slurp "day_04_camp_cleanup.txt")
                    str/split-lines
                    (map read-ranges)
                    (filter #(apply fully-contained? %))
                    count)))

;; part 2
(defn overlap? [p1 p2]
  (or (<= (first p1) (first p2) (second p1))
      (<= (first p1) (second p2) (second p1))
      (fully-contains? p2 p1)))

(assert (overlap? '(5 7) '(7 9)))
(assert (overlap? '(2 8) '(3 7)))
(assert (overlap? '(12 63) '(11 64)))
(assert (not (overlap? '(2 4) '(6 8))))
(assert (not (overlap? '(2 3) '(4 5))))

(is (= 4 (->> test-pairs
              str/split-lines
              (map read-ranges)
              (filter #(apply overlap? %))
              count)))

(is (= 931 (->> (slurp "day_04_camp_cleanup.txt")
                str/split-lines
                (map read-ranges)
                (filter #(apply overlap? %))
                count)))
