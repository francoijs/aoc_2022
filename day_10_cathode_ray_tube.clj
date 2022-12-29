;; https://adventofcode.com/2022/day/10
(ns day-10
  (:use clojure.test)
  (:require [clojure.string :as str]))

(def test-input
  "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(defn update-crt [st nst]
  (let [cycle (:cycle st)
        x (:x st)
        pixel (< (abs (- (mod (dec cycle) 40) x)) 2)]
;    (prn "cycle=" cycle ", x=" x "pixel=" pixel)
    (assoc-in nst [:crt (dec cycle)] pixel)))

(defn update-sum [st nst]
  (let [cycle (:cycle st)
        x (:x st)]
    (if (not= -1 (.indexOf (range 20 221 40) cycle))
      (assoc nst :sum (+ (:sum st) (* cycle x)))
      nst)))

(defn nop [st]
  (let [cycle (inc (:cycle st))]
    (update-crt st
                (update-sum st
                            (assoc st :cycle cycle)))))

(defn addx [val st]
  (let [x (+ val (:x st))
        cycle (inc (:cycle st))]
    (update-crt st
                (update-sum st
                            (assoc st
                                   :cycle cycle
                                   :x x)))))

(defn vector-grow [v len elem]
  (if (>= (count v) len) v
      (vector-grow (conj v elem) len elem)))
(is (= 3 (count (vector-grow '[] 3 0))))
(is (= 3 (count (vector-grow '[1 2] 3 0))))
(is (= 3 (count (vector-grow '[1 2 3] 3 0))))
(is (= 4 (count (vector-grow '[1 2 3] 4 0))))

(defn parse-instr [st s]
  (let [toks (str/split s #"[^-0-9]+")
        v (:program st)
        idx (:index st)]
;    (prn toks v idx)
    (if (zero? (count toks)) (assoc st
                                    :index (inc idx)
                                    :program (vector-grow v (inc idx) nop))
        (assoc st
               :index (+ 2 idx)
               :program (assoc (vector-grow v (+ 2 idx) nop)
                               (inc idx) nop
                               (+ 2 idx) (partial addx (parse-long (second toks))))))))

(defn read-program [input]
  (next (:program (->> input
                       str/split-lines
                       (reduce parse-instr
                               {:program '[]
                                :index 0})))))

(defn exec-program [prog]
  (reduce #(%2 %1)
          {:cycle 1 :x 1 :sum 0
           :crt (vec (repeat 240 false))}
          prog))

;; part 1
(is (= 13140 (-> test-input
                 read-program
                 exec-program
                 :sum)))
(is (= 11960 (-> (slurp "day_10_cathode_ray_tube.txt")
                 read-program
                 exec-program
                 :sum)))

;; part 2
(load-file "array.clj")
(->> (slurp "day_10_cathode_ray_tube.txt")
     read-program
     exec-program
     :crt
     (array/vector->array 40)
     array/print-array)
;; ####...##..##..####.###...##..#....#..#.
;; #.......#.#..#.#....#..#.#..#.#....#..#.
;; ###.....#.#....###..#..#.#....#....####.
;; #.......#.#....#....###..#.##.#....#..#.
;; #....#..#.#..#.#....#....#..#.#....#..#.
;; ####..##...##..#....#.....###.####.#..#.
