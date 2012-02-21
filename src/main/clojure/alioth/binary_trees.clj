;   Copyright (c) Rich Hickey and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;   Alioth benchmarks: http://shootout.alioth.debian.org/u64q/benchmark.php?test=binarytrees&lang=all
;   Inspired by http://shootout.alioth.debian.org/u64q/program.php?test=binarytrees&lang=java&id=1
;           and http://shootout.alioth.debian.org/u64q/program.php?test=binarytrees&lang=clojure&id=5                                        ;
(ns alioth.binary-trees
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const min-depth 4)

(deftype TreeNode [left right ^int item])

(defn ^:static make-tree [^long item ^long depth]
  (let [int-item (int item)
        int-depth (int depth)]
    (if (<= depth 0)
      (TreeNode. nil nil int-item)
      (TreeNode.
       (make-tree (unchecked-dec (unchecked-multiply (int 2) int-item))
                  (unchecked-dec int-depth))
       (make-tree (unchecked-multiply (int 2) int-item)
                  (unchecked-dec int-depth))
       int-item))))

(defn ^:static item-check ^long [^TreeNode node]
  (let [item (int (.item node))]
	  (if-not (.left node)
	    item
	    (unchecked-add 
         (unchecked-add 
           item 
           (unchecked-int (item-check (.left node))))
	       (unchecked-negate 
           (unchecked-int (item-check (.right node))))))))

    
(defn ^:static check-trees [^long i ^long acc ^long d]    
  (if (<= i 0)
    acc
    (let [value (unchecked-add 
                  (unchecked-int (item-check (make-tree i d)))
                  (unchecked-int (item-check (make-tree (- i) d))))]
      (recur (dec i) (+ acc value) d))))

(defn iterate-trees 
  ([mx mn d]
    (let [iterations (bit-shift-left 1 (int (+ mx mn (- d))))]
      (format "%d\t trees of depth %d\t check: %d" (* 2 iterations) d (check-trees iterations 0 d)))))

(defn main [max-depth]
  (let [stretch-depth (unchecked-inc (long max-depth))]
    (let [tree (make-tree (long 0) stretch-depth)
          check (item-check tree)]
      (println (format "stretch tree of depth %d\t check: %d" stretch-depth check)))
    (let [agents (repeatedly (.availableProcessors (Runtime/getRuntime)) #(agent []))
          long-lived-tree (make-tree 0 max-depth)]
      (loop [depth min-depth
             [a & more] (cycle agents)
             results []]
        (if (> depth stretch-depth)
          (doseq [r results] (println @r))
          (let [result (promise)]
            (send a (fn [_]
                      (deliver result (iterate-trees max-depth min-depth depth))))
            (recur (+ 2 depth) more (conj results result)))))
        (println (format "long lived tree of depth %d\t check: %d" max-depth (item-check long-lived-tree))))))

(defn -main [& args]
  (let [n (if (first args) (Integer/parseInt (first args)) 0)
        max-depth (if (> (+ min-depth 2) n) (+ min-depth 2) n)]
    (main max-depth)
    (shutdown-agents)))
