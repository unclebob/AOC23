(ns aoc2.core
  (:require [clojure.string :as string]))

(defn to-cube [cube-s]
  (let [parts (string/split cube-s #" ")
        n (Integer/parseInt (first parts))
        color (keyword (last parts))]
    [color n]))

(defn parse-set [set-s]
  (try
    (let [cubes-s (string/split set-s #",")
          cubes-s (map string/trim cubes-s)
          cubes (map to-cube cubes-s)]
      (into {} cubes))
    (catch Exception _
      {})))

(defn parse-game [game-s]
  (let [[id-s sets-s] (string/split game-s #":")
        id-s (subs id-s 5)
        id (Integer/parseInt id-s)
        sets-s (string/trim sets-s)
        sets-ls (string/split sets-s #";")
        sets-ls (map string/trim sets-ls)
        sets (map parse-set sets-ls)]
    [id sets]))

(defn parse-input [input]
  (let [games-ls (string/split-lines input)
        games (map parse-game games-ls)]
    (into {} games)))

(def constraints {:red 12 :green 13 :blue 14})

(defn valid-color? [[color count]]
  (let [constraint (get constraints color)]
    (<= count constraint)))

(defn valid-set? [set]
  (every? valid-color? set))

(defn valid-game? [game]
  (every? valid-set? game))

(defn valid-game-pair? [game-pair]
  (valid-game? (second game-pair)))

(defn valid-game-ids [games]
  (map first (filter valid-game-pair? games)))

(defn sum-valid-ids [file-name]
  (let [games (parse-input (slurp file-name))
        valid-ids (valid-game-ids games)]
    (reduce + valid-ids)))

(defn min-set [set1 set2]
  (let [r (max (get set1 :red 0) (get set2 :red 0))
        g (max (get set1 :green 0) (get set2 :green 0))
        b (max (get set1 :blue 0) (get set2 :blue 0))]
    {:red r :green g :blue b})
  )

(defn get-minimum-color-set [game]
  (reduce min-set game))

(defn sum-powers [file-name]
  (let [games (parse-input (slurp file-name))
        min-sets (map get-minimum-color-set (vals games))
        counts (map vals min-sets)
        powers (map #(reduce * %) counts)]
    (reduce + powers)))