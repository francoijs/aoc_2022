(ns day-20
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(defn ring-move [ring pos steps]
  (insert-at (drop-at ring pos)
             (mod (+ pos steps) (dec (count ring)))
             (get ring pos)))
(is (= '[2 1 3] (ring-move '[1 2 3] 0 1)))
(is (= '[1 3 2] (ring-move '[1 2 3] 2 -1)))
(is (= '[2 1 3] (ring-move '[1 2 3] 0 3)))
(is (= '[1 3 2] (ring-move '[1 2 3] 2 -5)))
;(is (= '[1 3 2] (ring-move '[1 2 3] 1 1))) ; same ordering but different 1st element

;; Gives the same ordering AND same 1st element (but it doesn't change the result of sum-coordinates)
;; (defn ring-move [ring pos steps]
;;   (let [nlen (dec (count ring))
;;         npos (+ pos steps)
;;         npos (if (or (zero? npos) (= nlen npos)) nlen
;;                  (mod npos nlen))]
;;     (insert-at (drop-at ring pos)
;;                npos
;;                (get ring pos))))

(defn read-ring [s]
  (->> (str/split s #"[ ,]")
       (map parse-long)
       (filter truthy?)
       vec))

(def test-mixing
  (->> "1, 2, -3, 3, -2, 0, 4
2, 1, -3, 3, -2, 0, 4
1, -3, 2, 3, -2, 0, 4
1, 2, 3, -2, -3, 0, 4
1, 2, -2, -3, 0, 3, 4
1, 2, -3, 0, 3, 4, -2
1, 2, -3, 0, 3, 4, -2
1, 2, -3, 4, 0, 3, -2"
       str/split-lines
       (map read-ring)))

(is (= (nth test-mixing 1) (ring-move (nth test-mixing 0) 0 1)))
(is (= (nth test-mixing 2) (ring-move (nth test-mixing 1) 0 2)))
(is (= (nth test-mixing 3) (ring-move (nth test-mixing 2) 1 -3)))
(is (= (nth test-mixing 4) (ring-move (nth test-mixing 3) 2 3)))
; (is (= (nth test-mixing 5) (ring-move (nth test-mixing 4) 2 -2))) ; same ordering but different 1st element
(is (= (nth test-mixing 6) (ring-move (nth test-mixing 5) 3 0)))
(is (= (nth test-mixing 7) (ring-move (nth test-mixing 6) 5 4)))

(defn mix-ring
  ([times ring]
   (loop [idx 0
          times times
          array (vec (map-indexed list ring))]
     (cond (zero? times) (mapv second array)
           (>= idx (count array)) (do (prn times)
                                      (recur 0
                                             (dec times)
                                             array))
           :else (let [pos (index-using #(= idx (first (first %))) array)
                       elem (second (get array pos))]
                   (recur (inc idx)
                          times
                          (ring-move array pos elem))))))
  
  ([ring] (mix-ring 1 ring)))

(def test-input "1
2
-3
3
-2
0
4")

(def test-ring (->> test-input
                    str/split-lines
                    (map parse-long)
                    vec))
(is (= '[-2 1 2 -3 4 0 3] (mix-ring test-ring)))

(defn sum-coordinates [ring]
  (let [idx (.indexOf ring 0)]
    (it-> (concat (subvec ring idx) (subvec ring 0 idx))
          (cycle it)
          (take 3001 it)
          (map #(nth it %) '(1000 2000 3000))
          (apply + it))))

;; part 1
(is (= 3 (->> test-ring
              mix-ring
              sum-coordinates)))

(is (= 11073 (->> (slurp "day_20_grove_positioning_system.txt")
                  str/split-lines
                  (mapv parse-long)
                  mix-ring
                  sum-coordinates)))

;; part 2
(is (= 1623178306 (->> test-ring
                       (mapv #(* 811589153 %))
                       (mix-ring 10)
                       sum-coordinates)))

;; FIXME: -4753 ?
;; (->> (slurp "day_20_grove_positioning_system.txt")
;;      str/split-lines
;;      (map parse-long)
;;      vec
;;      (mix-ring 10)
;;      sum-coordinates)
