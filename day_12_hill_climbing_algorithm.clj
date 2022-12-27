(ns day-12
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-input
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defn parse-area [input]
  (->> input
       str/split-lines
       (mapv #(str/split % #""))
       (mapv (fn [v] (mapv #(int (first %)) v)))
       to-array-2d))

;; Height and width of a 2d-array
(defn awidth [array]
  (if (zero? (alength array)) 0
      (count (get array 0))))
(def aheight alength)
(let [array (to-array-2d '[[1 2]
                           [3 4]
                           [5 6]])]
  (assert (= 2 (awidth array)))
  (assert (= 3 (aheight array))))

(defn neighbors [array x y]
  (reduce (fn [neigh delta]
            (let [nx (+ x (first delta))
                  ny (+ y (second delta))]
              (if (and (>= nx 0)
                       (>= ny 0)
                       (< nx (awidth array))
                       (< ny (aheight array))
                       (< (abs (- (aget array ny nx)
                                  (aget array y x))) 2))
                (cons (list nx ny) neigh)
                neigh)))
          '()
          '((-1 0) (0 -1) (0 1) (1 0))))

;; Convert an array to a graph of neighbors
(defn array->graph [array]
  (loop [x 0
         y 0
         graph {}]
    (cond (= y (aheight array)) graph
          (= x (awidth array)) (recur 0 (inc y) graph)
          :else (recur (inc x) y
                       (assoc graph
                              (list x y)
                              (neighbors array x y))))))

;; Return position '(x y) of first 'val found in the 'array.
(defn array-find [array val]
  (loop [x 0
         y 0]
    (cond (= y (aheight array)) nil
          (= x (awidth array)) (recur 0 (inc y))
          (= val (aget array y x)) (list x y)
          :else (recur (inc x) y))))

(load-file "a_star.clj")
(defn shortest-path [input]
  (let [array (parse-area input)
        start (array-find array (int \S))
        end   (array-find array (int \E))]
    (aset array (second start) (first start) (int \a))   ; S -> a
    (aset array (second end) (first end) (int \z))       ; E -> z
    (first (a-star/a* (array->graph array) start end))))


;; part 1
(load-file "timed.clj")
(is (= 31 (dec (count (timed (shortest-path test-input))))))
;; (count (timed (shortest-path (slurp "day_12_hill_climbing_algorithm.txt"))))
