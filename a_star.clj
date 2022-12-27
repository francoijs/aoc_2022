;; https://matthewdowney.github.io/astar-in-clojure-find-k-shortest-paths.html
(ns a-star
  (:use clojure.test))

(declare a*-seq, next-a*-path, unseen?, step-factory, rpath, cmp-step)

(defn a*
  "A sequence of paths from `src` to `dest`, shortest first, within the supplied `graph`.
  If the graph is weighted, supply a `distance` function. To make use of A*, supply a 
  heuristic function. Otherwise performs like Dijkstra's algorithm."
  [graph src dest & {:keys [distance heuristic]}]
  (let [init-adjacent (sorted-set-by cmp-step {:node src :cost 0 :entered 0})]
    (a*-seq graph dest init-adjacent
            (or distance (constantly 1))
            (or heuristic (constantly 0)))))

(defn a*-seq
  "Construct a lazy sequence of calls to `next-a*-path`, returning the shortest path first."
  [graph dest adjacent distance heuristic]
  (lazy-seq
    (when-let [[path, adjacent'] (next-a*-path graph dest adjacent distance heuristic)]
      (cons path (a*-seq graph dest adjacent' distance heuristic)))))

(defn next-a*-path [graph dest adjacent f-cost f-heur]
  (when-let [{:keys [node] :as current} (first adjacent)]
    (let [path (rpath current)
          adjacent' (disj adjacent current)] ;; "pop" the current node
      (if (= node dest)
        [(reverse path), adjacent']
        (let [last-idx (or (:entered (last adjacent')) 0)
              factory (step-factory current last-idx f-cost f-heur dest)
              xform (comp (filter (partial unseen? path)) (map-indexed factory))
              adjacent'' (into adjacent' xform (get graph node))]
          (recur graph dest adjacent'' f-cost f-heur))))))

(defn unseen? [path node]
  (not-any? #{node} path))

(defn step-factory [parent last-insertion cost heur dest]
  (fn [insertion-idx node]
    {:parent parent
     :node node
     :entered (+ last-insertion (inc insertion-idx))
     :cost (+ (:cost parent) (cost (:node parent) node) (heur node dest))}))

(defn rpath [{:keys [node parent]}]
  (lazy-seq
    (cons node (when parent (rpath parent)))))

(defn cmp-step [step-a step-b]
  (let [cmp (compare (:cost step-a) (:cost step-b))]
    (if (zero? cmp)
      (compare (:entered step-a) (:entered step-b))
      cmp)))

;; Testing
(let [graph 
      {"A" #{"B", "E"} 
       "B" #{"A", "C", "D"} 
       "C" #{"B", "D"} 
       "D" #{"B", "C", "E"}
       "E" #{"A", "D"}}
      costs
      {"A" {"B" 2, "E" 10}
       "B" {"A" 2, "C" 3, "D" 4}
       "C" {"B" 3, "D" 2}
       "D" {"B" 4, "C" 3, "E" 10}
       "E" {"A" 10, "D" 10}}
      cost (fn [node node']
             (get-in costs [node node']))]
  (is (= '(("A" "E" "D") ("A" "B" "D") ("A" "B" "C" "D")) (a* graph "A" "D")))
  (is (= '(("A" "B" "D") ("A" "B" "C" "D") ("A" "E" "D")) (a* graph "A" "D" :distance cost))))
