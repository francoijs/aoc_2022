(ns day-14
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-rocks "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

;; read a path
(defn read-path [line]
  (as-> line %
    (str/split % #"[^0-9]")
    (filter (comp not empty?) %)
    (map parse-long %)
    (partition 2 %)
    ))
(is (= '((498 4) (498 6) (496 6)) (read-path "498,4 -> 498,6 -> 496,6")))

;; convert a path to a list of segments (list of 2 points)
(defn path->segments [path]
  (partition 2 (concat (list (first path))
                       (mapcat #(list % %) (next (drop-last path)))
                       (list (last path)))))
(is (= '(((498 4) (498 6)) ((498 6) (496 6))) (path->segments (read-path "498,4 -> 498,6 -> 496,6"))))

;; convert a segment to a list of positions
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

;; height and width of a 2d-array
(defn aheight [array]
  (if (zero? (alength array)) 0
      (count (get array 0))))
(def awidth alength)
(let [array (to-array-2d '[[1 2]
                           [3 4]
                           [5 6]])]
  (assert (= 3 (awidth array)))
  (assert (= 2 (aheight array))))

;; build grid as a 2d-array from a list of paths
(defn build-grid [paths]
  (let [rocks (->> paths
                   (mapcat path->segments)
                   (mapcat segment->positions))
        width (inc (apply max (map first rocks)))
        height (inc (apply max (map second rocks)))]
    (reduce (fn [grid pos]
              (aset grid (first pos) (second pos) true)
              grid)
            (make-array Boolean/TYPE width height)
            rocks)))
(defn test-grid []
  (build-grid (->> test-rocks
                   str/split-lines
                   (map read-path))))

(assert (not (aget (test-grid) 498 3)))
(assert (aget (test-grid) 498 4))
(assert (= 504 (awidth (test-grid))))
(assert (= 10  (aheight (test-grid))))

;; simulate in the 'grid the fall a unit of sand located at the end of 'path
;; return new grid, new path, status of the unit of sand
(defn simulate-fall [grid path]
  (let [pos (last path)
        x-pos (first pos)
        y-pos (second pos)]
    (assert (not (aget grid x-pos y-pos)))
    (if (or (zero? x-pos)
            (= (dec (awidth grid)) x-pos)
            (= (dec (aheight grid)) y-pos))
      {:grid grid :path path :state :void}
      (let [below (aget grid x-pos (inc y-pos))
            below-left (aget grid (dec x-pos) (inc y-pos))
            below-right (aget grid (inc x-pos) (inc y-pos))]
        (cond (not below)       {:grid grid :path (conj path (list x-pos (inc y-pos)))       :state :fall}
              (not below-left)  {:grid grid :path (conj path (list (dec x-pos) (inc y-pos))) :state :fall}
              (not below-right) {:grid grid :path (conj path (list (inc x-pos) (inc y-pos))) :state :fall}
              :else (do
                      (aset grid x-pos y-pos true)
                      {:grid grid
                       :path path
                       :state (if (and (= (first (first path)) x-pos)
                                       (= (second (first path)) y-pos)) :full :rest)}))))))

(let [st (simulate-fall (test-grid) '[(500 0)])]
  (is (= :fall (:state st)))
  (is (= 2 (count (:path st)))))
(let [st (simulate-fall (test-grid) '[(500 8)])]
  (is (= :rest (:state st)))
  (is (aget (:grid st) 500 8))
  (is (= 1 (count (:path st)))))
(let [st (simulate-fall (test-grid) '[(494 8)])]
  (is (= :fall (:state st)))
  (is (= 2 (count (:path st)))))
(let [st (simulate-fall (test-grid) '[(493 9)])]
  (is (= :void (:state st)))
  (is (= 1 (count (:path st)))))

(defn simulate-loop [grid source]
  (loop [grid grid
         path (vec (list source))
         count 0]
    (let [st (simulate-fall grid path)]
      ;;          (prn count limit (:state st) (:path st))
      (case (:state st)
        :fall (recur (:grid st)
                     (:path st)
                     count)
        :rest (recur (:grid st)
                     (into [] (drop-last (:path st)))
                     (inc count))
        :full (inc count)
        count))))

;; part 1
(is (= 24 (simulate-loop (test-grid) '(500 0))))
(defn my-grid []
  (->> (slurp "day_14_regolith_reservoir.txt")
                              str/split-lines
                              (map read-path)
                              build-grid))
(is (= 774 (simulate-loop (my-grid) '(500 0))))

;; part 2
(defn build-grid-2 [paths]
  (let [rocks (->> paths
                   (mapcat path->segments)
                   (mapcat segment->positions))
        height (+ 3 (apply max (map second rocks)))       ; add room for
        width (+ height (apply max (map first rocks)))]   ; 'infinite' line
    (reduce (fn [grid pos]
              (aset grid (first pos) (second pos) true)
              grid)
            (make-array Boolean/TYPE width height)
            ;; add 'infinite' segment
            (concat rocks
                    (segment->positions (list (list 0 (dec height)) (list (dec width) (dec height))))))))
(defn test-grid-2 []
  (build-grid-2 (->> test-rocks
                     str/split-lines
                     (map read-path))))
(is (= 93 (simulate-loop (test-grid-2) '(500 0))))
(defn my-grid-2 []
  (->> (slurp "day_14_regolith_reservoir.txt")
                              str/split-lines
                              (map read-path)
                              build-grid-2))
(is (= 22499 (simulate-loop (my-grid-2) '(500 0))))
