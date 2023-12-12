(ns aoc11.core
  (:require [clojure.math.combinatorics :as combo]))

(defn expand-rows [universe]
  (loop [universe universe
         expanded []]
    (if (empty? universe)
      expanded
      (let [row (first universe)
            empty-row (apply str (repeat (count row) \E))]
        (if (every? #(or (= \. %) (= \E %)) row)
          (recur (rest universe) (concat expanded [empty-row]))
          (recur (rest universe) (concat expanded [row])))))))

(defn transpose [l] (map #(apply str %) (apply map vector l)))

(defn expand-universe [universe]
  (-> universe expand-rows transpose expand-rows transpose))

(defn get-cell
  ([universe x y]
   (get (get (vec universe) y) x))

  ([universe [x y]]
   (get-cell universe x y)))

(defn galaxy-coordinates [universe]
  (let [coords-and-nils (for [y (range (count universe))
                              x (range (count (first universe)))]
                          (if (= \# (get-cell universe x y))
                            [x y]
                            nil))]
    (remove nil? coords-and-nils)))

(defn galaxy-pairs [universe]
  (let [coords (galaxy-coordinates universe)
        pairs (combo/combinations coords 2)]
    pairs))

(defn galaxy-distance [[x1 y1] [x2 y2]]
  (let [maxx (max x1 x2)
        minx (min x1 x2)
        maxy (max y1 y2)
        miny (min y1 y2)]
    (+ (- maxx minx) (- maxy miny))))

(defn galaxy-distances [galaxy-pairs]
  (map #(apply galaxy-distance %) galaxy-pairs))

(defn sum-paths [universe]
  (let [e-universe (expand-universe universe)
        pairs (galaxy-pairs e-universe)
        distances (galaxy-distances pairs)]
    (reduce + distances)))

(defn next-step [[fx fy] [tx ty]]
  (cond
    (< fx tx) [(inc fx) fy]
    (> fx tx) [(dec fx) fy]
    (< fy ty) [fx (inc fy)]
    (> fy ty) [fx (dec fy)]
    :else nil
    )

  )

(defn mega-distance [universe expansion g1 g2]
  (loop [from g1
         to g2
         distance 0]
    (if (= from to)
      distance
      (let [step (next-step from to)
            cell (get-cell universe step)
            delta (if (= \E cell) expansion 1)]
        (recur step to (+ delta distance))))))

(defn mega-distances [universe delta galaxy-pairs]
  (map #(apply (partial mega-distance universe delta) %) galaxy-pairs))

(defn sum-mega-paths [universe delta]
  (let [e-universe (expand-universe universe)
        pairs (galaxy-pairs e-universe)
        distances (mega-distances e-universe delta pairs)]
    (reduce + distances)))