(ns aoc9.core
  (:require [clojure.string :as string]))

(defn parse-history-line [line]
  (let [nums-ls (string/split line #"\s+")]
    (map bigint nums-ls)))

(defn parse-history-lines [lines]
  (map parse-history-line lines))

(defn parse-history-file [file-name]
  (let [lines (string/split-lines (slurp file-name))]
    (parse-history-lines lines)))

(defn differentiate [nums]
  (map - (rest nums) nums))

(defn make-difference-table [nums]
  (loop [nums nums
         table []]
    (if (every? zero? nums)
      table
      (let [diffs (differentiate nums)]
        (recur diffs (conj table nums))))))

(defn next-value [nums]
  (loop [table (make-difference-table nums)
         val 0]
    (if (empty? table)
      val
      (let [dy (last (last table))]
        (recur (drop-last table) (+ val dy))))))

(defn previous-value [nums]
  (loop [table (make-difference-table nums)
         val 0]
    (if (empty? table)
      val
      (let [dy (first (last table))]
        (recur (drop-last table) (- dy val))))))

(defn sum-extrapolations [file-name]
  (let [histories (parse-history-file file-name)
        extrapolations (map next-value histories)]
    (reduce + extrapolations)))

(defn sum-predictions [file-name]
  (let [histories (parse-history-file file-name)
        predictions (map previous-value histories)]
    (reduce + predictions)))