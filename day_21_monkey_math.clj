(ns day-21
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-monkeys "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32")

(defn parse-node [s]
  (let [tokens (drop-if empty? (str/split s #"[ :]"))]
    (list (first tokens)
          (if (= 2 (count tokens)) {:leaf true :name (first tokens) :value (second tokens)}
              {:leaf false :name (first tokens) :op (nth tokens 2) :left (second tokens) :right (nth tokens 3)}))))

(defn read-ops [input]
  (reduce #(apply assoc %1 %2) {}
          (->> input
               str/split-lines
               (map parse-node))))

(defn build-tree [name ops]
  (let [node (get ops name)]
    (assert node)
    (if (:leaf node) node
        (assoc node
               :left  (build-tree (:left node) ops)
               :right (build-tree (:right node) ops)))))

(defn node->string [node]
  (if (:leaf node) (:value node)
      (str "(" (:op node) " "
           (node->string (:left node))  " "
           (node->string (:right node)) ")")))

(defn solve-node [node]
  (-> (node->string node)
      read-string
      eval))

;; part 1
(def test-tree (->> (read-ops test-monkeys)
                    (build-tree "root")))
(is (= 152 (solve-node test-tree)))

(is (= 56490240862410 (->> (slurp "day_21_monkey_math.txt")
                           read-ops
                           (build-tree "root")
                           solve-node)))


(defn depends-on-humn [node]
  (cond (= "humn" (:name node)) true
        (:leaf node) false
        :else (or (depends-on-humn (:left node))
                  (depends-on-humn (:right node)))))
(is (depends-on-humn test-tree))
(is (depends-on-humn (:left test-tree)))
(is (not (depends-on-humn (:right test-tree))))

(defn solve-for-humn [node result]
  (if (= "humn" (:name node))
    result
    (let [humn-left (depends-on-humn (:left node))
          humn-node (if humn-left (:left node) (:right node))
          other-val (solve-node (if humn-left (:right node) (:left node)))]
;      (prn result other-val)
      (case (:op node)
        "+" (solve-for-humn humn-node (- result other-val))
        "*" (solve-for-humn humn-node (/ result other-val))
        "-" (if humn-left
              (solve-for-humn humn-node (+ other-val result))
              (solve-for-humn humn-node (- other-val result)))
        "/" (if humn-left
              (solve-for-humn humn-node (* result other-val))
              (solve-for-humn humn-node (/ result other-val)))))))

;; part 2
(is (= 301 (solve-for-humn (assoc test-tree :op "-") 0)))

(is (= 3403989691757 (it-> (slurp "day_21_monkey_math.txt")
                           (read-ops it)
                           (build-tree "root" it)
                           (assoc it :op "-")
                           (solve-for-humn it 0))))
