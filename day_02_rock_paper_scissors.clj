(ns day-02
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-strategy "A Y
B X
C Z")

(defn shape-score [shape]
  (case shape
    \A 1   ; rock
    \B 2   ; paper
    \C 3)) ; scissors

(defn match-score [you opp]
  (+ (shape-score you)
     (cond (= you opp) 3
           (or (and (= you \A) (= opp \C))
               (and (= you \B) (= opp \A))
               (and (= you \C) (= opp \B))) 6
           :else 0)))

(defn strategy-score [s]
  (match-score (char (- (int (nth s 2)) (- (int \X) (int \A))))
               (first s)))
(is (= 8 (strategy-score "A Y")))
(is (= 1 (strategy-score "B X")))
(is (= 6 (strategy-score "C Z")))

;; part 1
(is (= 12586 (->> (slurp "day_02_rock_paper_scissors.txt")
                  str/split-lines
                  (map strategy-score)
                  (apply +))))

;; part 2
(defn strategy [s]
  (let [opp (first s)]
    (case (nth s 2)
      \Y opp                             ; draw
      \X (case opp \A \C \B \A \C \B)    ; lose
      \Z (case opp \A \B \B \C \C \A)))) ; win
(is (= \A (strategy "A Y")))
(is (= \A (strategy "B X")))
(is (= \A (strategy "C Z")))
  
(is (= 12 (->> test-strategy
               str/split-lines
               (map (fn [s] (match-score (strategy s) (first s))))
               (apply +))))
(is (= 13193 (->> (slurp "day_02_rock_paper_scissors.txt")
                  str/split-lines
                  (map (fn [s] (match-score (strategy s) (first s))))
                  (apply +))))
