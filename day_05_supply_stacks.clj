(ns day-05
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-stacks "
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 ")

(defn step [st c]
  (case (:state st)
    :read (case c
            \[ (assoc st :state :read-letter)
            \newline (assoc st :state :read :index 0)
            \space  (assoc st :state :one-space)
            st)
    :read-letter (assoc st
                        :state :read
                        :stacks (assoc (:stacks st) (:index st) (cons c (get (:stacks st) (:index st))))
                        :index (inc (:index st)))
    :one-space (case c
                 \space (assoc st :state :two-space)
                 \[ (assoc st :state :read-letter))
    :two-space (case c
                 \space (assoc st :state :three-space))
    :three-space (case c
                   \space (assoc st
                                 :state :read
                                 :index (inc (:index st))))))
(defn read-stacks [s]
  (let [lines (-> s
                  str/split-lines
                  reverse)
        num-stacks (-> lines
                       first
                       (str/split #"[0-9]")
                       count
                       dec)]
    (:stacks (reduce step
                     {:stacks (vec (repeat num-stacks '()))
                      :index  0
                      :state  :read}
                     (str/join "\n" (next lines))))))

(is (= '[(\N \Z) (\D \C \M) (\P)] (read-stacks test-stacks)))

(def test-instructions "move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn apply-instruction [stacks instr & same-order]
  (prn stacks instr)
  (let [instr (map parse-long (str/split instr #" "))
        src (dec (nth instr 3))
        dst (dec (nth instr 5))
        count (nth instr 1)
        crates (take count (get stacks src))]
    (prn crates same-order)
    (assoc stacks
           dst (concat (if same-order
                         crates
                         (reverse crates))
                       (get stacks dst))
           src (drop count (get stacks src)))))

(is (= '[(\D \N \Z) (\C \M) (\P)] (apply-instruction (read-stacks test-stacks)
                                                     (first (str/split-lines test-instructions)))))
(is (= '[(\C) (\M) (\Z \N \D \P)] (reduce apply-instruction
                                         (read-stacks test-stacks)
                                         (str/split-lines test-instructions))))

(defn read-message [stacks]
  (apply str (map first stacks)))

;; part 1
(is (= "CMZ" (read-message (reduce apply-instruction
                                   (read-stacks test-stacks)
                                   (str/split-lines test-instructions)))))
(let [input (partition-using (comp empty? first) (-> (slurp "day_05_supply_stacks.txt")
                                                     str/split-lines))
      stacks (read-stacks (str/join "\n" (first input)))
      instrs (next (second input))]
  (is (= "RFFFWBPNS" (read-message (reduce apply-instruction
                                           stacks instrs)))))

;; part 2
(defn apply-instruction-2 [stacks instr]
  (apply-instruction stacks instr true))

(is (= "MCD" (read-message (reduce apply-instruction-2
                                   (read-stacks test-stacks)
                                   (str/split-lines test-instructions)))))
(let [input (partition-using (comp empty? first) (-> (slurp "day_05_supply_stacks.txt")
                                                     str/split-lines))
      stacks (read-stacks (str/join "\n" (first input)))
      instrs (next (second input))]
  (is (= "CQQBBJFCS" (read-message (reduce apply-instruction-2
                                           stacks instrs)))))
