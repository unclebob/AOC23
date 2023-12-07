(ns aoc6.core
  (:require [clojure.string :as string]))

(defn parse-races [[time-line distance-line]]
  (let [times-ls (rest (string/split time-line #"\s+"))
        times (map #(Integer/parseInt %) times-ls)
        distances-ls (rest (string/split distance-line #"\s+"))
        distances (map #(Integer/parseInt %) distances-ls)]
    (apply map list [times distances])))

(defn parse-races-file [file-name]
  (parse-races (string/split-lines (slurp file-name))))

(defn race [t p]
  (* p (- t p)))

(defn races [t]
  (map (partial race t) (range (inc t))))

(defn winners [t record]
  (let [distances (races t)]
    (count (filter #(> % record) distances))))

(defn all-winners [records]
  (map #(apply winners %) records))

(defn product-of-all-winners [file-name]
  (let [records (parse-races-file file-name)]
    (reduce * (all-winners records))))

(defn parse-badly-kerned-race [file-name]
  (let [[time-line distance-line] (string/split-lines (slurp file-name))
        times-ls (rest (string/split time-line #"\s+"))
        time (bigint (apply str times-ls))
        distance-ls (rest (string/split distance-line #"\s+"))
        distance (bigint (apply str distance-ls))
        ]
    [time distance]
))
