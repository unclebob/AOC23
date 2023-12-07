(ns aoc3.core
  (:require [clojure.string :as string]))

(defn is-digit? [c]
  (<= (int \0) (int c) (int \9)))

(defn is-symbol? [c]
  (and
    (not (is-digit? c))
    (not= c \.)))

(defn get-char [schematic x y]
  (let [row (get schematic y)]
    (if (or (>= x (count row))
            (>= y (count schematic)))
      \.
      (get row x))))

(defn find-symbols [schematic]
  (filter some?
          (for [y (range (count schematic))
                x (range (count (get schematic y)))]
            (if (is-symbol? (get-char schematic x y))
              [x y]
              nil))))

(defn find-gears [schematic]
  (filter some?
          (for [y (range (count schematic))
                x (range (count (get schematic y)))]
            (if (= \* (get-char schematic x y))
              [x y]
              nil))))

(defn get-number [schematic x y]
  (let [row (get schematic y)
        start (subs row x)]
    (loop [s start
           l 0]
      (if (or (empty? s)
              (not (is-digit? (first s))))
        (let [n (Integer/parseInt (subs start 0 l))]
          [n l])
        (recur (subs s 1) (inc l))))))

(defn find-numbers [schematic]
  (loop [x 0
         y 0
         numbers []]
    (if (> y (count schematic))
      numbers
      (cond
        (>= x (count (get schematic y)))
        (recur 0 (inc y) numbers)

        (is-digit? (get-char schematic x y))
        (let [[number l] (get-number schematic x y)]
          (recur (+ x l) y (conj numbers [number [x y] l])))

        :else
        (recur (inc x) y numbers)))))

(defn box-number [[n [x y] l]]
  (let [tlx (max 0 (dec x))
        tly (max 0 (dec y))]
    [n [tlx tly] [(+ x l) (inc y)]]))

(defn is-adjacent? [[x y] [[tlx tly] [brx bry]]]
  (and
    (<= tlx x brx)
    (<= tly y bry)))

(defn get-adjacent-numbers [schematic]
  (let [symbols (find-symbols schematic)
        numbers (find-numbers schematic)
        boxes (map box-number numbers)]
    (filter some?
            (for [[n tl br] boxes]
              (do
                (if (some #(is-adjacent? % [tl br]) symbols)
                  n
                  nil))))))

(defn get-schematic [file-name]
  (string/split-lines (slurp file-name)))

(defn get-part-number-sum [file-name]
  (reduce + (get-adjacent-numbers (get-schematic file-name))))

(defn find-gear-ratios [schematic]
  (let [gears (find-gears schematic)
        numbers (find-numbers schematic)
        boxes (map box-number numbers)
        gear-pairs (for [gear gears]
                     (filter #(is-adjacent? gear (rest %)) boxes))
        gear-ratios (for [gear-pair gear-pairs]
                      (if (= 2 (count gear-pair))
                        (let [[[g1 _ _] [g2 _ _]] gear-pair]
                          (* g1 g2))
                        nil))]

    (filter some? gear-ratios)))

(defn sum-gear-ratios [file-name]
  (let [schematic (get-schematic file-name)
        gear-ratios (find-gear-ratios schematic)]
    (reduce + gear-ratios)))
