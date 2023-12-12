(ns aoc11.core-spec
  (:require [aoc11.core :refer :all]
            [clojure.string :as string]
            [speclj.core :refer :all]))

(def test-universe
  (string/split-lines
    "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#....."))
(def expanded-test-universe
  ["..E#.E..E." "..E..E.#E." "#.E..E..E." "EEEEEEEEEE" "..E..E#.E." ".#E..E..E." "..E..E..E#" "EEEEEEEEEE" "..E..E.#E." "#.E.#E..E."])

(describe "expansion"
  (it "expands the universe"
    (should= [] (expand-rows []))
    (should= ["#"] (expand-rows ["#"]))
    (should= ["E"] (expand-rows ["."]))
    (should= ["..#.."
              "EEEEE"
              "..#.."]
             (expand-rows ["..#.."
                           "....."
                           "..#.."]))

    (should= ["13" "24"] (transpose ["12" "34"]))
    (should= ["E"] (expand-universe ["."]))
    (should= ["##E#."
              "EEEEE"
              ".#E##"]
             (expand-universe ["##.#."
                               "....."
                               ".#.##"]))
    (should= expanded-test-universe (expand-universe test-universe))))

(describe "lengths"
  (it "should gather coordinates of galaxies"
    (should= #{[0 0] [1 1] [2 2]}
             (set (galaxy-coordinates ["#.."
                                       ".#."
                                       "..#"]))))
  (it "should get pairs"
    (should= #{[[0 0] [1 1]]}
             (set (galaxy-pairs ["#." ".#"])))
    (should= #{[[0 0] [1 1]] [[0 0] [0 2]] [[1 1] [0 2]]}
             (set (galaxy-pairs ["#." ".#" "#."]))))
  )

(describe "mega-distance"
  (it "makes next step"
    (should= [1 0] (next-step [0 0] [2 2]))
    (should= [2 0] (next-step [1 0] [2 2]))
    (should= [2 1] (next-step [2 0] [2 2]))
    (should= [2 2] (next-step [2 1] [2 2]))

    (should= [1 2] (next-step [2 2] [0 0]))
    (should= [0 2] (next-step [1 2] [0 0]))
    (should= [0 1] (next-step [0 2] [0 0]))
    (should= [0 0] (next-step [0 1] [0 0]))

    (should= nil (next-step [0 0] [0 0])))

  (it "should be normal if no E"
    (should= 2 (mega-distance ["#." ".#"] 1000000 [0 0] [1 1]))
    (should= 1000002 (mega-distance ["#E."
                                     ".E#"] 1000000 [0 0] [2 1]))))

(describe "solve part 1"
  (it "solves test data"
    (let [universe (string/split-lines (slurp "test-data"))]
      (should= 374 (sum-mega-paths universe 2))))

  (it "solves input"
    (let [universe (string/split-lines (slurp "input"))]
      (should= 9918828 (sum-mega-paths universe 2))))
  )

(describe "solution part 2"
  (it "solves test data"
    (let [universe (string/split-lines (slurp "test-data"))]
      (should= 8410 (sum-mega-paths universe 100))))

  (it "solves input"
    (let [universe (string/split-lines (slurp "input"))]
      (should= 692506533832 (sum-mega-paths universe 1000000))))
  )