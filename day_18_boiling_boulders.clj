(ns day-18
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def test-input
  "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")

(defn read-cubes [input]
  (->> input
       str/split-lines
       (map parse-coords)))

(defn parse-coords [s]
  (->> (str/split s #",")
       (map parse-long)
       (zipmap [:x :y :z])))

(defn neighbors? [c1 c2]
  (assert (not= c1 c2))
  (= 1 (apply + (map #(abs (- (% c1) (% c2))) '(:x :y :z)))))

(is (neighbors? (parse-coords "2,2,2") (parse-coords "1,2,2")))
(is (neighbors? (parse-coords "2,1,2") (parse-coords "1,1,2")))
(is (not (neighbors? (parse-coords "2,2,2") (parse-coords "1,1,2"))))

(defn surface-area [cubes]
  (apply + (map :area
                (reduce (fn [v pair]
                          (let [c1 (get v (first pair))
                                c2 (get v (second pair))]
                            (if (neighbors? c1 c2)
                              (update-in (update-in v [(first pair) :area] dec)
                                         [(second pair) :area] dec)
                              v)))
                        (mapv #(assoc % :area 6) cubes)
                        (combo/combinations (range (count cubes)) 2)))))

;; part 1
(is (= 64 (->> test-input
               read-cubes
               surface-area)))
(is (= 4548 (->> (slurp "day_18_boiling_boulders.txt")
                 read-cubes
                 surface-area)))
