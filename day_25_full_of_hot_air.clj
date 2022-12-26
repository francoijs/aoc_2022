(ns day-25
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-snafu
"  Decimal          SNAFU
        1              1
        2              2
        3             1=
        4             1-
        5             10
        6             11
        7             12
        8             2=
        9             2-
       10             20
       15            1=0
       20            1-0
     2022         1=11-2
    12345        1-0---0
314159265  1121-1110-1=0")

(defn- parse-snafu-digit [c]
  (case c
    \0 0
    \1 1
    \2 2
    \= -2
    \- -1))

(defn snafu->int [s]
  (if (empty? s) 0
      (+ (parse-snafu-digit (last s))
         (* 5 (snafu->int (drop-last s))))))

(defn int->snafu [n]
  (if (< n 3) (str (get (vec "=-012") (+ 2 n)))
      (str (int->snafu (quot (+ n 2) 5))
           (int->snafu (- (mod (+ n 2) 5) 2)))))

(->> test-snafu
     str/split-lines
     next
     (map #(str/split % #"[ ]"))
     (map #(drop-if str/blank? %))
     (map (fn [pair]
            (is (= (parse-long (first pair))
                   (snafu->int (second pair))))
            (is (= (int->snafu (parse-long (first pair)))
                   (second pair))))))

(def test-input
  "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122")

;; part 1
(is (= "2=-1=0" (->> test-input
                     str/split-lines
                     (map snafu->int)
                     (apply +)
                     int->snafu)))
(is (= "2==0=0===02--210---1" (->> (slurp "day_25_full_of_hot_air.txt")
                                   str/split-lines
                                   (map snafu->int)
                                   (apply +)
                                   int->snafu)))
