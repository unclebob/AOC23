(ns aoc1a.core
  (:require [clojure.string :as string]))

(defn is-digit? [c]
  (let [ic (int c)]
    (<= (int \0) ic (int \9))))

(defn filter-digits [s]
  (map str (filter is-digit? s)))

(defn unspell-digits [s]
  (loop [s s
         unspelled ""]
    (cond
      (empty? s) unspelled
      (string/starts-with? s "zero") (recur (subs s 1) (str unspelled "0"))
      (string/starts-with? s "one") (recur (subs s 1) (str unspelled "1"))
      (string/starts-with? s "two") (recur (subs s 1) (str unspelled "2"))
      (string/starts-with? s "three") (recur (subs s 1) (str unspelled "3"))
      (string/starts-with? s "four") (recur (subs s 1) (str unspelled "4"))
      (string/starts-with? s "five") (recur (subs s 1) (str unspelled "5"))
      (string/starts-with? s "six") (recur (subs s 1) (str unspelled "6"))
      (string/starts-with? s "seven") (recur (subs s 1) (str unspelled "7"))
      (string/starts-with? s "eight") (recur (subs s 1) (str unspelled "8"))
      (string/starts-with? s "nine") (recur (subs s 1) (str unspelled "9"))
      :else (recur (subs s 1) (str unspelled (first s)))
      )))

(defn get-calibration-value [s]
  (let [s (unspell-digits s)
        digits (filter-digits s)]
    (Integer/parseInt (str (first digits) (last digits)))))

(defn sum-calibrations [file-name]
  (let [lines (string/split-lines (slurp file-name))
        calibrations (map get-calibration-value lines)]
    (reduce + calibrations)))