(ns day-23
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-input "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..")

;; height and width of a 2d-array
(defn awidth [array]
  (if (zero? (alength array)) 0
      (count (get array 0))))
(def aheight alength)
(let [array (to-array-2d '[[1 2]
                           [3 4]
                           [5 6]])]
  (assert (= 2 (awidth array)))
  (assert (= 3 (aheight array))))

;; Read the grid (2d-array) and the elves positions (list)
;; Elves positions (x y) are relative to the center of the grid
(defn read-grid-and-elves [input]
  (let [st (reduce (fn [st line]
                     (assoc st
                            :grid (conj (:grid st)
                                        (mapv #(case % \. false true) line))
                            :elves (concat (:elves st)
                                           (->> line
                                                (map-indexed #(list (list %1 (:y st))
                                                                    (case %2 \. false true)))
                                                (filter second)
                                                (mapv first)))
                            :y (inc (:y st))))
                   {:grid '[] :y 0 :elves '()}
                   (str/split-lines input))
        elves (:elves st)
        grid (to-array-2d (:grid st))
        offset-x (quot (awidth  grid) 2)
        offset-y (quot (aheight grid) 2)]
    (list grid
          (map #(list (- (first %) offset-x)
                      (- (second %) offset-y))
               elves))))

(def move-dirs {:nw '(-1 -1) :n '(0 -1) :ne '(1 -1)
                :w  '(-1  0) :x '(0  0) :e  '(1  0)
                :sw '(-1  1) :s '(0  1) :se '(1  1)})
(def move-rules '[(:nw :n :ne)
                  (:sw :s :se)
                  (:nw :w :sw)
                  (:ne :e :se)])

;; Read at 'pos (position is relative to the center of the grid)
(defn read-pos [grid pos]
  (let [offset-x (quot (awidth  grid) 2)
        offset-y (quot (aheight grid) 2)]
    (aget grid
          (+ offset-y (second pos))
          (+ offset-x (first pos)))))

;; Set at 'pos (position is relative to the center of the grid)
(defn set-pos [grid pos val]
  (let [offset-x (quot (awidth  grid) 2)
        offset-y (quot (aheight grid) 2)]
    (aset grid
          (+ offset-y (second pos))
          (+ offset-x (first pos))
          val)))

;; New position in 'dir
(defn update-pos [pos dir]
  (map + pos (get move-dirs dir)))
(is (= '(10 21) (update-pos '(10 20) :s)))
(is (= '(11 19) (update-pos '(10 20) :ne)))

(defn nor
  ([a b c]
   (not (or a b c)))
  ([a b c d e f g h i j k l]
   (not (or a b c d e f g h i j k l))))   

;; Proposed direction from 'pos using 'rules
(defn candidate-dir [grid pos rules]
  ;; test if there are other elves around
  (if (apply nor (map #(read-pos grid (update-pos pos %))
                      (reduce concat '() rules)))
    :x
    (reduce (fn [dir rule]
              ;; test each direction in order
              (if (apply nor (map #(read-pos grid (update-pos pos %)) rule))
                (reduced (nth rule 1))
                dir))
            :x
            rules)))

(defn max-coord [pos]
  (apply max (map abs (list
                       (apply min (map first pos))
                       (apply max (map first pos))
                       (apply min (map second pos))
                       (apply max (map second pos))))))

;; Expand grid by 1 in each direction.
(defn grow-grid [grid]
  (let [ngrid (make-array Boolean/TYPE
                          (+ 2 (aheight grid))
                          (+ 2 (awidth grid)))]
    (loop [x 0
           y 0]
      (cond (= y (aheight grid)) ngrid
            (= x (awidth grid)) (recur 0 (inc y))
            :else (do
                    (aset ngrid (inc y) (inc x) (aget grid y x))
                    (recur (inc x) y))))))
(is (not (aget (grow-grid (to-array-2d '[[true]])) 0 0)))
(is (not (aget (grow-grid (to-array-2d '[[true]])) 2 2)))
(is (aget (grow-grid (to-array-2d '[[true]])) 1 1))
(is (= 3 (awidth (grow-grid (to-array-2d '[[true]])))))
(is (= 3 (aheight (grow-grid (to-array-2d '[[true]])))))

;; -3  0  3
;;  ....#.. -3
;;  ..###.#
;;  #...#.#
;;  .#...##  0
;;  #.###..
;;  ##.#.##
;;  .#..#..  3
(let [grid (grow-grid (first (read-grid-and-elves test-input)))]
  (is (read-pos grid '(1 -1)))
  (is (read-pos grid '(-3 1)))
  (is (not (read-pos grid '(2  1))))
  (is (not (read-pos grid '(-1 3))))
  (is (= :n (candidate-dir grid '(0 1) move-rules)))
  (is (= :n (candidate-dir grid '(-1 -2) move-rules)))
  (is (= :n (candidate-dir grid '(1 -3) move-rules)))
  (is (= :e (candidate-dir grid '(1 -2) move-rules)))
  (is (= :x (candidate-dir grid '(1 1) move-rules)))
  )

(defn print-grid [grid]
  (loop [x 0
         y 0]
    (cond (= y (aheight grid)) (prn)
          (= x (awidth grid)) (do (prn)
                                  (recur 0 (inc y)))
          :else (do (print (if (aget grid y x) \# \.))
                    (recur (inc x) y)))))

(defn unique? [ls elem]
  (< (count (filter #(= elem %) ls)) 2))
(is (unique? '(1 2 3) 2))
(is (not (unique? '(2 2 3) 2)))
(is (unique? '(2 2 3) 1))

;; 1-step update elves positions and grid
;; nil if no elf has moved
(defn update-grid [grid elves rules]
  (let [ngrid (grow-grid grid)
        dirs (mapv #(candidate-dir ngrid % rules) elves)  ; directions
        npos (mapv #(update-pos %1 %2) elves dirs)]       ; new positions
;    (prn dirs)
    (if (apply = (conj dirs :x)) nil                      ; no new position (all dirs are :x)
        (do (prn "still elves %" (/ (count (filter #(= % :x) dirs))
                                    (count dirs)
                                    0.01))
            (list ngrid
                  (reduce (fn [nelves idx]
                            (let [elf (nth elves idx)
                                  nelf (get npos idx)]
                              (cons (if (unique? npos nelf)
                                      (do ;(prn "moving " elf " to " nelf)
                                        (set-pos ngrid elf false)
                                        (set-pos ngrid nelf true)                                
                                        nelf)
                                      elf)
                                    nelves)))
                          '()
                          (range (count elves))))))))

(defn rotate [v pos]
  (vec (concat (subvec v pos)
               (subvec v 0 pos))))
(is (= '[2 3 1] (rotate '[1 2 3] 1)))
(is (= '[3 1 2] (rotate '[1 2 3] 2)))

(defn simulate [input times]
  (loop [[grid elves] (read-grid-and-elves input)
         rules move-rules
         times times]
    (if (and grid (> times 0))
      (recur (update-grid grid elves rules)
             (rotate rules 1)
             (dec times))
      elves)))

;; (simulate ".....
;; ..##.
;; ..#..
;; .....
;; ..##.
;; ....." 10)

(defn count-empty-tiles [input]
  (let [pos (simulate input 10)
        x-min (apply min (map first pos))
        x-max (apply max (map first pos))
        y-min (apply min (map second pos))
        y-max (apply max (map second pos))]
    (- (* (- x-max x-min -1)
          (- y-max y-min -1))
       (count pos))))

;; part 1
(is (= 110 (count-empty-tiles test-input)))
(is (= 4000 (count-empty-tiles (slurp "day_23_unstable_diffusion.txt"))))


;; part 2
(defn find-final-round [input]
  (loop [[grid elves] (read-grid-and-elves input)
         rules move-rules
         times 0]
    (if (not grid) times
        (recur (update-grid grid elves rules)
               (rotate rules 1)
               (inc times)))))

(is (= 20 (find-final-round test-input)))

;; FIXME: too slow...
;; (find-final-round (slurp "day_23_unstable_diffusion.txt"))
