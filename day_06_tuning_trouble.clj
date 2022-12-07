(ns day-06
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(defn search-marker [len s]
  (reduce (fn [st c]
            (let [pattern (:pattern st)
                  index (:index st)
                  pos (str/index-of pattern c)]
;;              (prn pattern c)
              (cond pos {:index (inc index)
                         :pattern (str (subs pattern (inc pos)) c)}
                    (= (dec len) (count pattern)) (reduced (inc index))
                    :else {:index (inc index)
                           :pattern (str pattern c)})))
          {:index 0 :pattern ""}
          s))

(def test-signal-1 "bvwbjplbgvbhsrlpgdmjqwftvncz")
(def test-signal-2 "nppdvjthqldpwncqszvftbrmjlhg")
(def test-signal-3 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
(def test-signal-4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
(is (= 5 (search-marker 4 test-signal-1)))
(is (= 6 (search-marker 4 test-signal-2)))
(is (= 10 (search-marker 4 test-signal-3)))
(is (= 11 (search-marker 4 test-signal-4)))

;; part 1
(is (= 1282 (search-marker 4 (slurp "day_06_tuning_trouble.txt"))))

;; part 2
(is (= 26 (search-marker 14 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))
(is (= 3513 (search-marker 14 (slurp "day_06_tuning_trouble.txt"))))
