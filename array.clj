(ns array
  (:use tupelo.core
        clojure.test))

;; Height and width of a 2d-array
(def aheight alength)
(defn awidth [array]
  (if (zero? (alength array)) 0
      (count (get array 0))))

(let [array (to-array-2d '[[1 2]
                           [3 4]
                           [5 6]])]
  (assert (= 2 (awidth array)))
  (assert (= 3 (aheight array))))

;; Return position '(x y) of first 'val found in a 2d-array
(defn array-find [array val]
  (loop [x 0
         y 0]
    (cond (= y (aheight array)) nil
          (= x (awidth array)) (recur 0 (inc y))
          (= val (aget array y x)) (list x y)
          :else (recur (inc x) y))))

;; Convert 1d vector to 2d-array
(defn vector->array [width v]
  (assert (zero? (mod (count v) width)))
  (to-array-2d (mapv vec (partition width v))))

(let [array (vector->array 2 '[1 2 3 4 5 6])]
  (assert (= 2 (awidth array)))
  (assert (= 3 (aheight array))))

;; Print content of binary array
(defn print-array [array]
  (loop [x 0
         y 0]
    (cond (= y (aheight array)) (prn)
          (= x (awidth array)) (do (prn)
                                   (recur 0 (inc y)))
          :else (do (print (if (aget array y x) \# \.))
                    (recur (inc x) y)))))
