(ns aoc3.core-spec
  (:require [speclj.core :refer :all]
            [aoc3.core :refer :all]))

(describe "utilities"
  (it "identifies digits"
    (should (is-digit? \0))
    (should (is-digit? \9))
    (should-not (is-digit? \.))
    (should-not (is-digit? \@)))

  (it "gets chars"
    (should= \1 (get-char ["1"] 0 0))
    (should= \2 (get-char [".." ".2.."] 1 1))
    (should= \. (get-char [".."] 2 0))
    (should= \. (get-char [".."] 0 1)))

  (it "identifies symbols"
    (should (every? is-symbol? "#*$+%/"))
    (should (not-any? is-symbol? "0123456789.")))

  (it "finds symbols"
    (should= [] (find-symbols []))
    (should= [[0 0]] (find-symbols ["@"]))
    (should= [[1 1]] (find-symbols [".." ".@"]))
    (should= [[0 0] [1 1] [2 2]] (find-symbols ["@..." ".@" "..@..."])))

  (it "finds numbers"
    (should= [0 1] (get-number ["0"] 0 0))
    (should= [55 2] (get-number ["..." "..55@."] 2 1))
    (should= [] (find-numbers []))
    (should= [[1 [0 0] 1]] (find-numbers ["1"]))
    (should= [[55 [1 1] 2]] (find-numbers ["..." ".55##"]))
    (should= [[55 [1 1] 2] [999 [5 1] 3]] (find-numbers ["..." ".55##999"])))

  (it "makes boxes around numbers"
    (should= [1 [0 0] [2 2]]
             (box-number [1 [1 1] 1]))
    (should= [99 [0 0] [2 1]]
             (box-number [99 [0 0] 2])))

  (it "checks adjacency"
    (should (is-adjacent? [0 0] [[0 0] [1 1]]))
    (should-not (is-adjacent? [1 1] [[2 1] [3 2]])))

  (it "finds numbers adjacent to symbols"
    (should= [] (get-adjacent-numbers []))
    (should= [99] (get-adjacent-numbers ["99#"]))
    (should= [88 99] (get-adjacent-numbers ["...#..."
                                            "....88..."
                                            "$99..."]))
    (should= [] (get-adjacent-numbers [".#.99.#"])))
  )

(describe "acceptance tests 1"
  (it "finds the adjacent numbers in the test data"
    (should= [467 35 633 617 592 755 664 598]
           (get-adjacent-numbers (get-schematic "test-data")))))

(describe "solution 1"
  (it "solves part 1"
    (should= 4361 (get-part-number-sum "test-data"))
    (should= 514969 (get-part-number-sum "input"))))

(describe "gears in part 2"
  (it "finds gears"
    (should= [[3 2] [5 3]] (find-gears ["..." ".#.." "...*" ".....*"])))

  (it "finds gear ratios"
    (should= [27] (find-gear-ratios ["..." "3*9" ".."]))
    (should= [12 20] (find-gear-ratios ["..." "3*4*5" "..."]))
    (should= [] (find-gear-ratios ["..*10..."]))
    (should= [] (find-gear-ratios ["99*22"
                                   "...33"]))))

(describe "solution part 2"
  (it "solves part 2"
    (should= 467835 (sum-gear-ratios "test-data"))
    (should= nil (sum-gear-ratios "input"))))



