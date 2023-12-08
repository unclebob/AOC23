(ns aoc8.core
  (:require [clojure.string :as string]))

(defn parse-node [node-s]
  (let [items (string/split node-s #"=")
        [name children-s] (map string/trim items)
        children-s-len (count children-s)
        children-s (subs children-s 1 (dec children-s-len))
        children (string/split children-s #",")
        children (map string/trim children)]
    [name children]))

(defn parse-nodes [nodes-ls]
  (loop [nodes-ls nodes-ls
         nodes {}]
    (cond (empty? nodes-ls)
          nodes

          (empty? (string/trim (first nodes-ls)))
          (recur (rest nodes-ls) nodes)

          :else
          (let [[name children] (parse-node (first nodes-ls))]
            (recur (rest nodes-ls) (assoc nodes name children)))
          )))

(defn parse-map [file-name]
  (let [lines (string/split-lines (slurp file-name))
        directions (first lines)
        nodes (parse-nodes (rest lines))]
    [directions nodes]))

(defn count-steps [file-name]
  (let [[directions nodes] (parse-map file-name)]
    (loop [node "AAA"
           directions (cycle directions)
           steps 0]
      (if (= node "ZZZ")
        steps
        (let [direction (first directions)
              [left right] (get nodes node)
              dest (if (= \L direction) left right)]
          (recur dest (rest directions) (inc steps)))))))

(defn count-paths [node-map start directions]
  (let [limit (* (count (keys node-map)) (count directions))]
    (loop [node start
           directions (cycle directions)
           steps #{}
           terminators []
           step-count 0]
      (let [[left right] (get node-map node)
            dest (if (= (first directions) \L) left right)]
        (cond
          (or (> step-count limit)
              (some #(= node %) terminators))
          steps

          (= \Z (last node))
          (recur dest
                 (rest directions)
                 (conj steps step-count)
                 (conj terminators node)
                 (inc step-count))

          :else
          (recur dest
                 (rest directions)
                 steps
                 terminators
                 (inc step-count)))))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (if (or (zero? a) (zero? b))
    0
    (let [gcd-ab (gcd a b)]
      (if (zero? gcd-ab)
        0
        (bigint (/ (* a b) gcd-ab))))))

(defn count-concurrent-steps [file-name]
  (let [[directions node-map] (parse-map file-name)
        starting-nodes (filter #(= \A (last %)) (keys node-map))
        steps (map #(count-paths node-map % directions) starting-nodes)]
    (reduce lcm (map #(bigint (first %)) steps))))

