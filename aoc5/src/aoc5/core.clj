(ns aoc5.core
  (:require [clojure.string :as string]))

(defn parse-ints [ints-s]
  (let [ints-s (string/trim ints-s)
        ints-ls (string/split ints-s #"\s+")]
    (map bigint ints-ls)))

(defn parse-seeds [seed-line]
  (let [[_ seeds-s] (string/split seed-line #":")]
    (parse-ints seeds-s)))

(defn parse-map [maps map-lines]
  (let [map-descriptor (first map-lines)
        [map-name _] (string/split map-descriptor #"\s+")
        map-key (keyword map-name)]
    (loop [map-lines (rest map-lines)
           map-l []]
      (if (empty? (first map-lines))
        [(assoc maps map-key map-l) (rest map-lines)]
        (recur (rest map-lines) (conj map-l (parse-ints (first map-lines))))))))

(defn parse-almanac [lines]
  (let [seed-line (first lines)
        map-lines (rest lines)]
    (loop [map-lines map-lines
           maps {:seeds (parse-seeds seed-line)}]
      (cond
        (empty? map-lines)
        maps

        (empty? (first map-lines))
        (recur (rest map-lines) maps)

        :else
        (let [[maps map-lines] (parse-map maps map-lines)]
          (recur map-lines maps))))))

(defn map-to-range [x [dest source n]]
  (if (<= source x (+ source n -1))
    (+ dest (- x source))
    x))

(defn map-to-ranges [x ranges]
  (cond
    (empty? ranges)
    x

    (not= x (map-to-range x (first ranges)))
    (map-to-range x (first ranges))

    :else
    (recur x (rest ranges))))

(defn map-sources-to-ranges [sources ranges]
  (map #(map-to-ranges % ranges) sources))

(def dependency-list [:seed-to-soil
                      :soil-to-fertilizer
                      :fertilizer-to-water
                      :water-to-light
                      :light-to-temperature
                      :temperature-to-humidity
                      :humidity-to-location])

(defn read-almanac [file-name]
  (parse-almanac (string/split-lines (slurp file-name))))

(defn find-nearest-location [file-name]
  (let [almanac (read-almanac file-name)]
    (loop [sources (:seeds almanac)
           map-keys dependency-list]
      (if (empty? map-keys)
        (apply min sources)
        (let [ranges (get almanac (first map-keys))
              mappings (map-sources-to-ranges sources ranges)]
          (recur mappings (rest map-keys)))))))

(defn map-source-range-to-dest [[start sn] [dest source dn]]
  (let [end (+ start sn -1)
        source-end (+ source dn -1)]
    (cond
      (< end source)
      [[] [[start sn]]]

      (> start (+ source dn -1))
      [[] [[start sn]]]

      (and (< start source) (> end source-end))
      (let [left-n (- source start)
            right-n (- end source-end)]
        [[dest dn] [[start left-n] [(+ start left-n dn) right-n]]])

      (and (< start source) (>= end source))
      (let [match-n (- end source -1)]
        [[dest match-n] [[start (- sn match-n)]]])

      (and (>= start source) (<= end source-end))
      (let [ds (- start source)]
        [[(+ dest ds) sn] []])

      (and (<= start source-end) (> end source-end))
      (let [match-n (- source-end start -1)
            ds (- start source)]
        [[(+ dest ds) match-n] [[(+ start match-n) (- sn match-n)]]])

      :else
      nil)))

(defn map-source-ranges-to-dest [source-ranges dest-map]
  (loop [source-ranges source-ranges
         matched []
         unmatched []]
    (if (empty? source-ranges)
      [matched unmatched]
      (let [[this-match this-unmatch] (map-source-range-to-dest (first source-ranges) dest-map)
            new-match (if (empty? this-match) matched (conj matched this-match))]
        (recur (rest source-ranges) new-match (concat unmatched this-unmatch))))))

(defn map-source-ranges-to-all-dests [source-ranges dest-maps]
  (loop [dest-maps dest-maps
         matched []
         unmatched source-ranges]
    (if (empty? dest-maps)
      (set (concat matched unmatched))
      (let [[this-matched this-unmatched] (map-source-ranges-to-dest
                                            unmatched (first dest-maps))]
        (recur (rest dest-maps)
               (remove empty? (concat matched this-matched))
               this-unmatched)))))

(defn find-nearest-location-using-seed-ranges [file-name]
  (let [almanac (read-almanac file-name)
        seed-ranges (partition 2 (:seeds almanac))]
    (loop [source-ranges seed-ranges
           map-keys dependency-list]
      (if (empty? map-keys)
        (apply min (map first source-ranges))
        (let [ranges (get almanac (first map-keys))
              mapped-ranges (map-source-ranges-to-all-dests source-ranges ranges)]
          (recur mapped-ranges (rest map-keys)))))))