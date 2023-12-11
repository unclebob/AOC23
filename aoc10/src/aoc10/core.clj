(ns aoc10.core
  (:require [clojure.string :as string]))

(defn get-cell
  ([map x y]
   (get (get map y) x))

  ([map [x y]]
   (get-cell map x y)))

(defn find-start [pipe-map]
  (let [h (count pipe-map)
        w (count (first pipe-map))]
    (loop [cell-coords (for [x (range w) y (range h)] [x y])]
      (let [[x y] (first cell-coords)]
        (cond
          (empty? cell-coords) nil
          (= \S (get-cell pipe-map x y)) [x y]
          :else (recur (rest cell-coords)))))))

(defn neighbors [[x y]]
  #{[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]})

(defn direction [[from-x from-y] [to-x to-y]]
  (let [delta [(- to-x from-x) (- to-y from-y)]]
    (condp = delta
      [0 1] :south
      [1 0] :east
      [0 -1] :north
      [-1 0] :west
      nil)))

(defn move [[from-x from-y] dir]
  (condp = dir
    :north [from-x (dec from-y)]
    :south [from-x (inc from-y)]
    :east [(inc from-x) from-y]
    :west [(dec from-x) from-y]))


(defn next-cell [pipe-map from to]
  (let [dir (direction from to)
        pipe (get-cell pipe-map to)]
    (condp = [dir pipe]
      [:north \|] (move to :north)
      [:south \|] (move to :south)
      [:east \-] (move to :east)
      [:west \-] (move to :west)
      [:north \7] (move to :west)
      [:east \7] (move to :south)
      [:north \F] (move to :east)
      [:west \F] (move to :south)
      [:south \L] (move to :east)
      [:west \L] (move to :north)
      [:south \J] (move to :west)
      [:east \J] (move to :north)
      nil)))

(defn walk [pipe-map start to]
  (loop [from start
         to to
         next-node (next-cell pipe-map from to)
         path [from]]
    (cond
      (= to start) path
      (nil? next-node) nil
      :else (recur to next-node
                   (next-cell pipe-map to next-node)
                   (conj path to)))))

(defn find-loop [pipe-map]
  (let [start (find-start pipe-map)
        tos (neighbors start)
        paths (map #(walk pipe-map start %) tos)]
    (first (remove nil? paths)))
  )

(defn farthest [file-name]
  (let [pipe-map (string/split-lines (slurp file-name))
        loop (find-loop pipe-map)]
    (/ (count loop) 2)))

(defn slope-intercept [[[x1 y1] [x2 y2]]]
  (let [dy (- y1 y2)
        dx (- x1 x2)
        m (/ dy dx)
        b (- y1 (* m x1))]
    [m b]))

(defn vertical? [[[x1 _] [x2 _]]]
  (= x1 x2))

(defn intersect-lines [l1 l2]
  (cond
    (and (vertical? l1) (vertical? l2))
    nil

    (vertical? l1)
    (let [[m b] (slope-intercept l2)
          x (ffirst l1)
          y (+ b (* m x))]
      [x y])

    (vertical? l2)
    (let [[m b] (slope-intercept l1)
          x (ffirst l2)
          y (+ b (* m x))]
      [x y])

    :else
    (let [[m1 b1] (slope-intercept l1)
          [m2 b2] (slope-intercept l2)]
      (if (zero? (- m1 m2))
        nil
        (let [x (/ (- b2 b1) (- m1 m2))
              y (+ (* m1 x) b1)]
          [x y])))))

(defn is-on-segment? [[[x1 y1] [x2 y2]] [x y]]
  (let [minx (min x1 x2)
        maxx (max x1 x2)
        miny (min y1 y2)
        maxy (max y1 y2)]
    (and
      (<= minx x maxx)
      (<= miny y maxy))))

(defn intersect-segments [l1 l2]
  (let [intersection (intersect-lines l1 l2)]
    (if (and (some? intersection)
             (is-on-segment? l1 intersection)
             (is-on-segment? l2 intersection))
      intersection
      nil)))

(defn is-in? [the-loop x y]
  (if (contains? (set the-loop) [x y])
    false
    (let [end-points (rest (concat the-loop (take 1 the-loop)))
          lines (map (fn [p1 p2] [p1 p2]) the-loop end-points)
          max-shift (max x y)
          test-line [[x (- y 0.5)] [(- x max-shift) (- y max-shift 0.5)]]
          intersections (map #(intersect-segments test-line %) lines)]
      (odd? (count (remove nil? intersections))))))

(defn area [pipe-map]
  (let [the-loop (find-loop pipe-map)
        in-out (for [y (range (count pipe-map))
                     x (range (count (first pipe-map)))]
                 (is-in? the-loop x y))]
    (count (filter identity in-out))))