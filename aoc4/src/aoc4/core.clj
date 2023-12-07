(ns aoc4.core
  (:require
    [clojure.set :as set]
    [clojure.string :as string]))

(defn parse-number-set [numbers-s]
  (if (empty? numbers-s)
    #{}
    (let [numbers-ls (string/split numbers-s #"\s+")]
      (into #{} (map #(Integer/parseInt %) numbers-ls)))))

(defn parse-card [line]
  (let [[card-s contents-s] (string/split line #":")
        [winners-s have-s] (string/split contents-s #"\|")
        winners-s (string/trim winners-s)
        have-s (string/trim have-s)
        [_ id-s] (string/split card-s #"\s+")
        id (Integer/parseInt id-s)
        winners (parse-number-set winners-s)
        have (parse-number-set have-s)]
    [id [winners have]]))

(defn parse-cards [lines]
  (loop [lines lines
         cards {}]
    (if (empty? lines)
      cards
      (recur (rest lines) (apply (partial assoc cards)
                                 (parse-card (first lines)))))))

(defn card-value [[winners have]]
  (let [wins (set/intersection winners have)]
    (int (Math/pow 2 (dec (count wins))))))

(defn card-matches [[winners have]]
  (let [wins (set/intersection winners have)]
    (count wins)))

(defn sum-card-values [file-name]
  (let [lines (string/split-lines (slurp file-name))
        cards (parse-cards lines)
        sets (vals cards)
        card-values (map card-value sets)]
    (reduce + card-values)))

(defn make-counts [card-map]
  (let [ids (keys card-map)
        pairs (map #(vector % 1) ids)]
    (into {} pairs)))

(defn update-counts [counts id matches n]
  (if (zero? matches)
    counts
    (recur (update counts (inc id) + n) (inc id) (dec matches) n)))

(defn count-all-cards [file-name]
  (let [lines (string/split-lines (slurp file-name))
        cards (parse-cards lines)
        counts (make-counts cards)]
    (loop [id 1 counts counts]
      (if (nil? (get cards id))
        (reduce + (vals counts))
        (let [card (get cards id)
              card-count (get counts id)
              matches (card-matches card)
              counts (update-counts counts id matches card-count)]
          (recur (inc id) counts))))))