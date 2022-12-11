(ns day-08
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-trees "30373
25512
65332
33549
35390")

(defn read-trees [lines]
  (reduce (fn [result line]
            (conj result
                  (vec (map #(- (int %) (int \0))
                            line))))
          '[]
          lines))
(def trees (read-trees (str/split-lines test-trees)))

(defn update-line [st idx]
  (let [height (get (:heights st) idx)]
    (if (> height (:max st))
      (assoc st
             :line (assoc (:line st) idx true)
             :max height)
      st)))
(update-line {:line (vec (repeat 5 false))
              :max 0
              :heights (get trees 0)} 0)

(defn update-visibles [visibles trees]
  (loop [idx 0
         result '[]]
    (if (>= idx (count visibles))
      result
      (let [line (get visibles idx)]
        (recur (inc idx)
               (conj result 
                     (:line (reduce update-line
                                    {:line line
                                     :max -1
                                     :heights (get trees idx)}
                                    (range (count trees))))))))))

(defn rotate-right [array]
  (loop [i 0
         res '[]]
    (if (= i (count array)) res
        (recur (inc i)
               (conj res (vec (reverse (mapv #(get % i) array))))))))
(rotate-right '[[1 2 3] [4 5 6] [7 8 9]])

(defn count-visible-trees [heights]
  (let [n (count heights)
        visibles (vec (repeat n (vec (repeat n false))))]
    (loop [visibles visibles
           heights heights
           pass 0]
      ;;      (println "pass" pass)
      (if (= 4 pass)
        (count (filter true? (apply concat visibles)))
        (recur (rotate-right (update-visibles visibles heights))
               (rotate-right heights)
               (inc pass))))))

;; part 1
(is (= 21 (count-visible-trees trees)))
(is (= 1543 (-> (slurp "day_08_treetop_tree_house.txt")
                str/split-lines
                read-trees
                count-visible-trees)))

;; part 2
(defn scenic-view [heights row col source-height dir]
  (let [nmax (dec (alength heights))
        height (aget heights row col)
        next-row (case dir
                   :up (dec row)
                   :down (inc row)
                   row)
        next-col (case dir
                   :left (dec col)
                   :right (inc col)
                   col)]
;;    (prn dir nmax row col next-row next-col)
    (cond (or (and (= dir :up) (zero? row))
              (and (= dir :down) (= nmax row))
              (and (= dir :left) (zero? col))
              (and (= dir :right) (= nmax col))) 0
          (>= (aget heights next-row next-col) source-height) 1
          :else (inc (scenic-view heights next-row next-col source-height dir)))))

(defn scenic-score [heights row col]
  (let [heights (to-array-2d heights)]
    (apply * (map (partial scenic-view heights row col (aget heights row col))
                  '(:up :down :left :right)))))

(is (= 4 (scenic-score trees 1 2)))
(is (= 8 (scenic-score trees 3 2)))

(defn find-max-score [heights]
  (let [n (count heights)]
    (apply max (map #(scenic-score heights
                                   (quot % n)
                                   (mod % n))
                    (range (* n n))))))
(is (= 8 (find-max-score trees)))

(is (= 595080 (-> (slurp "day_08_treetop_tree_house.txt")
                  str/split-lines
                  read-trees
                  find-max-score)))

