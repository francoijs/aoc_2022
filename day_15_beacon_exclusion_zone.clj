(ns day-15
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-input
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

;; Parse sector into a map '(x y radius) with the "manhattan radius" of the sector
(defn read-sector [s]
  (let [toks (str/split s #"[^0-9]+")
        xs (parse-long (nth toks 1))
        ys (parse-long (nth toks 2))
        xb (parse-long (nth toks 3))
        yb (parse-long (nth toks 4))
        d (+ (abs (- xs xb))
             (abs (- ys yb)))]
    {:x xs
     :y ys
     :radius d
     :beacon (list xb yb)}))

;; Area is a vector of 'rows (a row is a list of non-overlapping intervals)
;; First row has ordinate 'y-min
(defn make-area [sectors]
  (let [y-min (apply min (map #(- (:y %) (:radius %)) sectors))
        y-max (apply max (map #(+ (:y %) (:radius %)) sectors))
        h (- y-max y-min -1)]
    {:y-min y-min
     :rows (vec (repeat h '()))
     :beacons (map :beacon sectors)}))

;; Merge a 2d interval 'intv into a row (list of intervals) 'ls
(defn merge-interval [ls intv]
  (if (empty? ls)
    (list intv)
    (let [car (first ls)]
      (cond (> (first intv) (second car)) (cons car (merge-interval (next ls) intv))
            (< (second intv) (first car)) (cons intv ls)
            :else (merge-interval (next ls)
                                  (list (min (first intv) (first car))
                                        (max (second intv) (second car))))))))

(is (= '((1 2) (3 4)) (merge-interval '((1 2)) '(3 4))))
(is (= '((1 2) (3 4)) (merge-interval '((3 4)) '(1 2))))
(is (= '((1 4)) (merge-interval '((1 3)) '(2 4))))
(is (= '((1 4)) (merge-interval '((1 2) (3 4)) '(2 3))))

;; Merge a sector '(x y d) into an area
(defn merge-sector [area sector]
  (loop [d (:radius sector)
         area area]
    (if (< d 0) area
        (let [x (:x sector)
              y (:y sector)
              y0 (:y-min area)
              delta-y (- (:radius sector) d)
              -d (- y delta-y y0)
              +d (- (+ y delta-y) y0)
              intv (list (- x d) (+ x d))]
;;          (prn y0 -d +d intv)
          (recur (dec d)
                 (assoc-in (assoc-in area
                                     [:rows -d]
                                     (merge-interval (get (:rows area) -d) intv))
                           [:rows +d]
                           (merge-interval (get (:rows area) +d) intv)))))))

;; (merge-sector (->> test-input
;;                    str/split-lines
;;                    (map read-sector)
;;                    make-area)
;;               {:x 8 :y 7 :radius 9})

(def test-sectors (->> test-input
                       str/split-lines
                       (map read-sector)))
(def test-area (make-area test-sectors))

(defn area-get-row [area y]
  (get (:rows area) (- y (:y-min area))))

(defn count-candidate-positions [area y]
  (let [row (area-get-row area y)
        beacons (filter #(= y (second %)) (:beacons area))]
    (prn row beacons)
    (- (apply + (map #(- (second %) (first %) -1)
                     row))
       (count (dedupe (sort #(< (second %1) (second %2)) beacons))))))


;; part 1
(is (= 26 (count-candidate-positions (reduce merge-sector
                                             test-area
                                             test-sectors) 10)))
        
;; (let [sectors (->> (slurp "day_15_beacon_exclusion_zone.txt")
;;                    str/split-lines
;;                    (map read-sector))
;;       area (make-area sectors)]
;;   (prn (count sectors) (count (:rows area)) (count (:beacons area))))
;;   (count-candidate-positions (reduce merge-sector area sectors) 10))
