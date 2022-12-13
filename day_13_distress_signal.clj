(ns day-13
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-packets "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(defn read-packet-pairs [input]
  (->> input
       str/split-lines
       (partition-using (comp empty? first))
       (map #(map read-string (take-last 2 %)))))

(defn ordered? [left right]
  (cond (and (integer? left) (integer? right)) (<= left right)
        (integer? left) (ordered? (list left) right)
        (integer? right) (ordered? left (list right))
        (empty? right) false
        (empty? left) true
        (= (first left) (first right)) (ordered? (next left) (next right))
        :else (ordered? (first left) (first right))))

(assert (ordered? '[1,1,3,1,1] '[1,1,5,1,1]))
(assert (ordered? '[[1],[2,3,4]] '[[1],4]))
(assert (not (ordered? '[9] '[[8,7,6]])))
(assert (ordered? '[[4,4],4,4] '[[4,4],4,4,4]))
(assert (not (ordered? '[7,7,7,7] '[7,7,7])))
(assert (ordered? '[] '[3]))
(assert (not (ordered? '[[[]]] '[[]])))
(assert (not (ordered? '[1,[2,[3,[4,[5,6,7]]]],8,9] '[1,[2,[3,[4,[5,6,0]]]],8,9])))

(defn count-ordered-pairs [pairs]
  (loop [sum 0
         idx 1
         pairs pairs]
    (if (empty? pairs) sum
        (recur (if (apply ordered? (first pairs)) (+ sum idx) sum)
               (inc idx)
               (next pairs)))))

;; part 1
(is (= 13 (-> test-packets
              read-packet-pairs
              count-ordered-pairs)))

(is (= 5330 (-> (slurp "day_13_distress_signal.txt")
                read-packet-pairs
                count-ordered-pairs)))

;; part 2
(defn read-packets [input]
  (->> input
       str/split-lines
       (filter (comp not empty?))
       (map read-string)))

(defn decoder-key [packets]
  (let [sorted (->> packets
                    (concat '([[2]] [[6]]))
                    (sort ordered?))]
    (* (inc (.indexOf sorted '[[2]]))
       (inc (.indexOf sorted '[[6]])))))

(is (= 140 (decoder-key (read-packets test-packets))))
(is (= 27648 (->> (slurp "day_13_distress_signal.txt")
                  read-packets
                  decoder-key)))


;; condensed version:
(let [dividers '([[2]] [[6]])
	  sorted (->> (slurp "day_13_distress_signal.txt")
				  clojure.string/split-lines
				  (filter (comp not empty?))
				  (map read-string)
				  (concat dividers)
                  (sort (fn ord? [l r]
						  (cond (and (integer? l) (integer? r)) (<= l r)
								(integer? l) (ord? (list l) r)
								(integer? r) (ord? l (list r))
								(empty? r) false
								(empty? l) true
								(= (first l) (first r)) (ord? (next l) (next r))
								:else (ord? (first l) (first r))))))]
  (apply * (map #(inc (.indexOf sorted %)) dividers)))
