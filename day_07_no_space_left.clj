(ns day-07
  (:use tupelo.core
        clojure.test)
  (:require [clojure.string :as str]))

(def test-input
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")


(defn insert-node [tree path name size]
  (assert (= \/ (last path)))
  (cond
    (some #(= (str path name) (:path %)) (:children tree)) tree                 
    (= (:path tree) path) (update tree :children conj {:path (str path name)
                                                       :size size})
    :else (assoc tree :children (map #(insert-node % path name size) (:children tree)))))

(is (= {:path "/" :children '({:path "/a" :size 1})} 
       (insert-node {:path "/"} "/" "a" 1)))
;; do nothing if node already there
(is (= {:path "/" :children '({:path "/a/" :size 0})} 
       (insert-node {:path "/" :children '({:path "/a/" :size 0})} "/" "a/" 0)))
(is (= {:path "/a/" :children '({:path "/a/b" :size 2})} 
       (insert-node {:path "/a/"} "/a/" "b" 2)))

(defn parent-path [path]
  (assert (= \/ (last path)))
  (if (= path "/") path
      (subs path 0 (-> path
                       (subs 0 (dec (count path)))
                       (str/last-index-of "/")
                       inc))))

(is (= "/" (parent-path "/")))
(is (= "/" (parent-path "/a/")))
(is (= "/a/b/" (parent-path "/a/b/c/")))

(defn step [ctx tok]
  ;;  (prn ctx tok)
  (case (:state ctx)
    :new-line (if (= tok "$") 
                (assoc ctx :state :command)
                (throw (ex-info "invalid input" {:token tok})))
    :command (cond (= tok "cd") (assoc ctx :state :cd-arg)
                   (= tok "ls") (assoc ctx :state :ls-output)
                   :else (throw (ex-info "invalid command" {:token tok})))
    :cd-arg (cond (= tok "..") (assoc ctx :state :new-line
                                      :path (parent-path (:path ctx)))
                  (= tok "/") (assoc ctx :state :new-line
                                     :path "/")
                  :else (assoc ctx :state :new-line
                               :tree (insert-node (:tree ctx) (:path ctx) 
                                                  (str tok "/") 0)
                               :path (str (:path ctx) tok "/")))
    :ls-output (cond (= tok "$") (assoc ctx :state :command)
                     (= tok "dir") (assoc ctx :state :dir-name)
                     :else (assoc ctx :state :file-name
                                  :file-size (parse-long tok)))
    :dir-name (assoc ctx :state :ls-output
                     :tree (insert-node (:tree ctx) (:path ctx) (str tok "/") 0))
    :file-name (assoc ctx :state :ls-output
                      :tree (insert-node (:tree ctx) (:path ctx) tok (:file-size ctx)))
    (throw (ex-info "invalid state" {:state (:state ctx)}))))

(def context0 {:state :new-line
               :tree {:path "/" :size 0}
               :path "/"})

(defn build-tree [instrs]
  (:tree (reduce step context0 instrs)))

(is (= {:path "/" :size 0 :children '({:path "/a/" :size 0})}
       (build-tree '("$" "cd" "a"))))
(is (= {:path "/" :size 0}
       (build-tree '("$" "cd" "/"))))
(is (= {:path "/" :size 0 
        :children '({:path "/a/" :size 0
                     :children ({:path "/a/b/" :size 0})})}
       (build-tree '("$" "cd" "a" "$" "cd" "b"))))
(is (= {:path "/" :size 0 
        :children '({:path "/a/" :size 0})}
       (build-tree '("$" "cd" "a" "$" "cd" ".."))))
(is (= {:path "/" :size 0 
        :children '({:path "/a/" :size 0})}
       (build-tree '("$" "cd" "a" "$" "cd" ".." "$" "cd" "a"))))
(is (= {:path "/" :size 0 
        :children '({:path "/b/" :size 0}
                    {:path "/a/" :size 0})}
       (build-tree '("$" "cd" "a" "$" "cd" ".." "$" "cd" "b"))))
(is (= {:path "/" :size 0 :children '({:path "/a/" :size 0})}
       (build-tree '("$" "ls" "dir" "a"))))
(is (= {:path "/" :size 0 :children '({:path "/a" :size 123})}
       (build-tree '("$" "ls" "123" "a"))))

(defn total-size [tree]
  (+ (:size tree) (apply + (map total-size (:children tree)))))

(is (= 3 (total-size {:path "/" :size 0
                      :children '({:path "/a" :size 1}
                                  {:path "/b" :size 2})})))
(def test-instrs (-> test-input
                     (str/split #"[ \n]")))
(is (= 48381165 (total-size (build-tree test-instrs))))

(defn find-nodes [pred tree]
  (concat (if (pred tree) (list tree) '())
          (mapcat #(find-nodes pred %) (:children tree))))

(is (= '({:path "/a" :size 1}) (find-nodes #(= 1 (:size %)) {:path "/"
                                                             :children '({:path "/a" :size 1}
                                                                         {:path "/b" :size 2})})))
(is (= '({:path "/a" :size 1}
         {:path "/b/c" :size 1}) (find-nodes #(= 1 (:size %)) '{:path "/"
                                                                :children ({:path "/a" :size 1}
                                                                           {:path "/b" :size 2
                                                                            :children ({:path "/b/c" :size 1})})})))

;; part 1
(defn sum-sizes [pred tree]
  (->> tree
       (find-nodes pred)
       (map total-size)
       (apply +)))

(def max-100k #(and (<= (total-size %) 100000)
                    (zero? (:size %))))
(is (= 95437 (sum-sizes max-100k (build-tree test-instrs))))
(is (= 1118405 (-> (slurp "day_07_no_space_left.txt")
                   (str/split #"[ \n]")
                   build-tree
                   (->> (sum-sizes max-100k)))))

;; part 2
(defn min-size [tree]
  (let [unused (- 70000000 (total-size tree))
        needed (- 30000000 unused)]
    (->> tree
         (find-nodes #(and (>= (total-size %) needed)
                           (zero? (:size %))))
         (sort-by total-size)
         first)))

(is (= "/d/" (:path (min-size (build-tree test-instrs)))))
(is (= 24933642 (total-size (min-size (build-tree test-instrs)))))

(is (= 12545514 (-> (slurp "day_07_no_space_left.txt")
                    (str/split #"[ \n]")
                    build-tree
                    min-size
                    total-size)))
