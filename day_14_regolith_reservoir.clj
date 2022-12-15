(ns day-14
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-rocks "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn read-path [line]
  (as-> line %
    (str/split % #"[^0-9]")
    (filter (comp not empty?) %)
    (map parse-long %)
    (partition 2 %)
    ))
(is (= '((498 4) (498 6) (496 6)) (read-path "498,4 -> 498,6 -> 496,6")))

(defn segment->positions [path]
  (let [x1 (first (first path))
        y1 (second (first path))
        x2 (first (second path))
        y2 (second (second path))]
    (cond (< y1 y2) (map #(list x1 %) (range y1 (inc y2)))
          (> y1 y2) (map #(list x1 %) (range y1 (dec y2) -1))
          (< x1 x2) (map #(list % y1) (range x1 (inc x2)))
          :else     (map #(list % y1) (range x1 (dec x2) -1)))))
(is (= '((498 4)
         (498 5)
         (498 6)) (segment->positions '((498 4) (498 6)))))
(is (= '((6 498)
         (5 498)
         (4 498)) (segment->positions '((6 498) (4 498)))))

(defn path->segments [path]
  (partition 2 (concat (list (first path))
                       (mapcat #(list % %) (next (drop-last path)))
                       (list (last path)))))
(is (= '(((498 4) (498 6)) ((498 6) (496 6))) (path->segments (read-path "498,4 -> 498,6 -> 496,6"))))

(defn awidth [array]
  (if (zero? (alength array)) 0
      (count (get array 0))))
(is (= 3 (awidth (to-array-2d '[[1 2 3]
                                [4 5 6]]))))

(def aheight alength)
(is (= 2 (aheight (to-array-2d '[[1 2 3]
                                 [4 5 6]]))))

(defn build-grid [paths]
  (let [rocks (->> paths
                   (mapcat path->segments)
                   (mapcat segment->positions))
        width (inc (apply max (map second rocks)))
        height (inc (apply max (map first rocks)))]
    (reduce (fn [grid pos]
              (let [idx (+ (* width (first pos)) (second pos))]
                (aset grid (first pos) (second pos) true)
                grid))
            (make-array Boolean/TYPE height width)
            rocks)))
(def test-grid (build-grid (->> test-rocks
                                str/split-lines
                                (map read-path))))
(assert (not (aget test-grid 498 3)))
(assert (aget test-grid 498 4))

(defn simulate-fall [grid path]
  (let [pos (last path)
        x-pos (first pos)
        y-pos (second pos)]
    (prn (aheight grid) (awidth grid))
    (if (or (zero? x-pos)
            (= (dec (awidth grid)) x-pos)
            (= (dec (aheight grid)) y-pos))
      (list grid path :void)
      (let [below (aget grid x-pos (inc y-pos))
            below-left (aget grid (dec x-pos) (inc y-pos))
            below-right (aget grid (inc x-pos) (inc y-pos))]
        (cond (not below) (list grid (conj path (list x-pos (inc y-pos))) :fall)
              (not below-left) (list grid (conj path (list (dec x-pos) (inc y-pos)) :fall))
              (not below-right)  (list grid (conj path (list (inc x-pos) (inc y-pos)) :fall))
              :else (do
                      (aset grid x-pos y-pos true)
                      (list grid path :rest)))))))
(is (= :fall (nth (simulate-fall grid '[(500 0)]) 2)))
(is (= :rest (nth (simulate-fall grid '[(500 8)]) 2)))
(is (= :void (nth (simulate-fall grid '[(493 9)]) 2)))
