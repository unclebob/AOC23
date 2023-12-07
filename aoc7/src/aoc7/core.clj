(ns aoc7.core
  (:require [clojure.string :as string]))

(defn parse-hand [hand-s]
  (let [[hand bid-s] (string/split hand-s #"\s+")]
    [hand (Integer/parseInt bid-s)]))

(defn parse-hands [file-name]
  (let [hands-ls (string/split-lines (slurp file-name))]
    (map parse-hand hands-ls)))

(defn type-of-hand [hand]
  (let [freqs (frequencies hand)
        freqs-j (dissoc freqs \*)
        freq-counts (sort (vals freqs-j))
        jokers (get freqs \* 0)
        last-freq (if (= jokers 5) 5 (+ (last freq-counts) jokers))
        freq-counts (concat (butlast freq-counts) [last-freq])
        ]
    (cond
      (= 1 (last freq-counts)) 1
      (= 5 (last freq-counts)) 7
      (= [1 2] (take-last 2 freq-counts)) 2
      (= [2 2] (take-last 2 freq-counts)) 3
      (= [1 3] (take-last 2 freq-counts)) 4
      (= [2 3] (take-last 2 freq-counts)) 5
      (= [1 4] (take-last 2 freq-counts)) 6
      :else nil)))

(def card-rank {\* -1
                \2 0
                \3 1
                \4 2
                \5 3
                \6 4
                \7 5
                \8 6
                \9 7
                \T 8
                \J 9
                \Q 10
                \K 11
                \A 12})

(defn greater-card [c1 c2]
  (> (get card-rank c1)
     (get card-rank c2)))

(defn greater-cards-in-sequence [h1 h2]
  (loop [pairs (partition 2 (interleave h1 h2))]
    (cond
      (empty? pairs) false
      (apply greater-card (first pairs)) true
      (apply = (first pairs)) (recur (rest pairs))
      :else false)))

(defn greater-hand [h1 h2]
  (let [t1 (type-of-hand h1)
        t2 (type-of-hand h2)]
    (cond
      (> t1 t2) true
      (= t1 t2) (greater-cards-in-sequence h1 h2)
      :else
      false)))

(defn hand-comparator [[h1 _] [h2 _]]
  (cond (greater-hand h1 h2) 1
        (= h1 h2) 0
        :else -1))

(defn sort-hands-with-bids [hands-with-bids]
  (sort hand-comparator hands-with-bids))

(defn product-of-bids-by-rank [file-name]
  (let [hands (parse-hands file-name)
        sorted-hands (sort-hands-with-bids hands)
        bids-and-ranks (map-indexed (fn [i h] [(inc i) (second h)]) sorted-hands)
        scores (map #(apply * %) bids-and-ranks)]
    (reduce + scores)))

(defn jokerize [hand]
  (string/replace hand \J \*))

(defn product-of-bids-by-rank-with-jokers [file-name]
  (let [hands (parse-hands file-name)
        hands (map (fn [[hand bid]] [(jokerize hand) bid]) hands)
        sorted-hands (sort-hands-with-bids hands)
        bids-and-ranks (map-indexed (fn [i h] [(inc i) (second h)]) sorted-hands)
        scores (map #(apply * %) bids-and-ranks)]
    (reduce + scores)))