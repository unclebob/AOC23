(ns aoc10.core-spec
  (:require [aoc10.core :refer :all]
            [clojure.string :as string]
            [speclj.core :refer :all]))
(def test-map ["....."
               ".S-7."
               ".|.|."
               ".L-J."
               "....."])

(def small-map ["S7"
                "LJ"])
(describe "map"
  (it "accesses the map with coordinates"
    (should= \S (get-cell test-map 1 1))
    (should= \S (get-cell test-map [1 1]))
    (should= \J (get-cell test-map 3 3))
    (should= nil (get-cell test-map 5 0))
    (should= nil (get-cell test-map 0 5))
    (should= nil (get-cell test-map -1 -1)))

  (it "finds start"
    (should= [1 1] (find-start test-map)))

  (it "finds neighbors"
    (should= #{[0 1] [2 1] [1 0] [1 2]} (neighbors [1 1])))

  (it "finds direction"
    (should= :east (direction [0 0] [1 0]))
    (should= :west (direction [1 0] [0 0]))
    (should= :south (direction [0 0] [0 1]))
    (should= :north (direction [0 1] [0 0])))

  (it "moves"
    (should= [1 0] (move [1 1] :north))
    (should= [1 1] (move [1 0] :south))
    (should= [2 1] (move [1 1] :east))
    (should= [1 1] (move [2 1] :west))
    )

  (it "finds next cell"
    (should= [1 1] (next-cell small-map [0 0] [1 0]))
    (should= [0 1] (next-cell small-map [1 0] [1 1]))
    (should= [0 0] (next-cell small-map [1 1] [0 1]))
    )

  (it "walks the map"
    (should= [[0 0] [1 0] [1 1] [0 1]] (walk small-map [0 0] [1 0]))
    (should= [[1 1] [2 1] [3 1] [3 2] [3 3] [2 3] [1 3] [1 2]]
             (walk test-map [1 1] [2 1]))
    (should= [[0 0] [0 1] [1 1] [1 0]] (walk small-map [0 0] [0 1]))
    (should= nil (walk test-map [1 1] [1 0]))
    (should= nil (walk test-map [1 1] [0 1]))
    (should= [[1 1] [1 2] [1 3] [2 3] [3 3] [3 2] [3 1] [2 1]]
             (walk test-map [1 1] [1 2])))

  (it "finds the loop"
    (should= [[1 1] [2 1] [3 1] [3 2] [3 3] [2 3] [1 3] [1 2]]
             (find-loop test-map))))

(describe "solve part 1"
  (it "solves test data"
    (should= 8 (farthest "test-data")))
  (it "solves input"
    (should= 6812 (farthest "input"))))

(def test2-map ["..-.."
                "LF-7F"
                "|S.|-"
                ".L-J."
                "...|."])

(def test-loop (find-loop test-map))
(def test2-loop (find-loop test2-map))

(describe "area"
  (it "intersects lines"
    (should= [1 1] (intersect-lines [[1 0] [1 2]] [[0 1] [2 1]]))
    (should= [1 1] (intersect-segments [[0 1] [2 1]] [[1 0] [1 2]]))
    (should= [1 1] (intersect-lines [[0 0] [2 2]] [[0 2] [2 0]]))
    (should= nil (intersect-lines [[0 0] [2 2]] [[1 1] [3 3]]))
    (should= nil (intersect-segments [[0 0] [1 1]] [[10 10] [11 12]]))
    (should= [5.5 2.0] (intersect-segments [[5 2] [6 2]] [[7 3.5] [0 -3.5]]))
    (should= [5.5 2.0] (intersect-lines [[7 3.5] [0 -3.5]] [[5 2] [4 2]]))
    )

  (it "detects in in or out"
    (should-not (is-in? test2-loop 1 1))
    (should-not (is-in? test2-loop 3 3))
    (should-not (is-in? test2-loop 4 2))
    (should (is-in? test2-loop 2 2)))

  (it "finds area of a map"
    (should= 1 (area test-map))
    (should= 1 (area test2-map))))

(describe "solution part 2"
  (it "solves test data"
    (should= 4 (area (string/split-lines (slurp "test-area")))))

  (it "solves input"
    (should= 527 (area (string/split-lines (slurp "input")))))
  )


