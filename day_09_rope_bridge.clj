(ns day-9
  (:use clojure.test)
  (:require [clojure.string :as str]))

(def test-input-1
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defn sign [n]
  (cond
    (> 0 n) -1
    (> n 0) 1
    :else 0))

(defn update-tx [tx ty hx hy]
  (let [dx (- hx tx)
        dy (- hy ty)]
    (if (and (< (abs dx) 2) (< (abs dy) 2)) tx
        (+ tx (sign dx)))))

(defn update-ty [tx ty hx hy]
  (let [dx (- hx tx)
        dy (- hy ty)]
    (if (and (< (abs dx) 2) (< (abs dy) 2)) ty
        (+ ty (sign dy)))))

(defn step [ctx line]
  (loop [ctx ctx
         dir (get line 0)
         num (Integer/parseInt (subs line 2))
         idx 0]
    (cond 
      (zero? num) ctx
      ;; the whole tail has been updated
      (>= idx (count (:tx ctx))) (recur
                                  (update ctx :grid conj (list (last (:tx ctx))
                                                               (last (:ty ctx))))
                                  dir (dec num) 0)
      ;; update head and tail at 0
      (zero? idx) (let [hx (+ (:hx ctx) (case dir \L -1 \R 1 0))
                        hy (+ (:hy ctx) (case dir \U -1 \D 1 0))
                        tx (update-tx (first (:tx ctx))
                                      (first (:ty ctx))
                                      hx hy)
                        ty (update-ty (first (:tx ctx))
                                      (first (:ty ctx))
                                      hx hy)]
                    ;;(prn num idx ctx)
                    (recur 
                     (assoc ctx :hx hx :hy hy
                            :tx (assoc (:tx ctx) 0 tx)
                            :ty (assoc (:ty ctx) 0 ty))
                     dir num (inc idx)))
      ;; update tail at idx
      :else (let [hx (get (:tx ctx) (dec idx))
                  hy (get (:ty ctx) (dec idx))
                  tx (update-tx (get (:tx ctx) idx)
                                (get (:ty ctx) idx)
                                hx hy)
                  ty (update-ty (get (:tx ctx) idx)
                                (get (:ty ctx) idx)
                                hx hy)]
              ;;(prn num idx ctx)
              (recur 
               (assoc ctx
                      :tx (assoc (:tx ctx) idx tx)
                      :ty (assoc (:ty ctx) idx ty))
               dir num (inc idx))))))

;; part 1
(defn count-tail-pos
  ([input len]
   (->> input
        str/split-lines
        (reduce step
                {:grid (set '())
                 :hx 0 :hy 0
                 :tx (vec (repeat len 0))
                 :ty (vec (repeat len 0))})
        :grid
        count))
  ([input] (count-tail-pos input 1)))

(is (= 13 (count-tail-pos test-input-1)))
(is (= 6391 (count-tail-pos (slurp "day_09_rope_bridge.txt"))))

;; part 2
(is (= 1 (count-tail-pos test-input-1 9)))

(def test-input-2
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(is (= 36 (count-tail-pos test-input-2 9)))
(is (= 2593 (count-tail-pos (slurp "day_09_rope_bridge.txt") 9)))
