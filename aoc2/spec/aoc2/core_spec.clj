(ns aoc2.core-spec
  (:require [aoc2.core :refer :all]
            [speclj.core :refer :all]))

(describe "parser"
  (it "parses sets"
    (should= {} (parse-set ""))
    (should= {:blue 3 :red 4} (parse-set "3 blue, 4 red")))

  (it "parses-games"
    (should= [1 [{:blue 3 :red 4} {:red 9 :green 7}]]
             (parse-game "Game 1: 3 blue, 4 red; 9 red, 7 green")))

  (it "parses-input"
    (should= {1 [{:blue 3 :red 4} {:red 9 :green 7}]
              2 [{:blue 5} {:red 1 :green 2 :blue 3}]}
             (parse-input "Game 1: 3 blue, 4 red; 9 red, 7 green\nGame 2: 5 blue; 1 red, 2 green, 3 blue")))
  )

(describe "games"
  (it "validates a game"
    (should (valid-game? [{}]))
    (should (valid-game? [{:blue 3 :red 4} {:red 9 :green 7}]))
    (should (valid-game? [{:red 12 :green 13 :blue 14}]))
    (should-not (valid-game? [{:red 13 :green 13 :blue 14}]))
    (should-not (valid-game? [{:red 12 :green 14 :blue 14}]))
    (should-not (valid-game? [{:red 12 :green 13 :blue 15}])))

  (it "finds valid games"
    (should= [22] (valid-game-ids {22 [{}]}))
    (should= [1 3] (valid-game-ids {1 [{}]
                                    2 [{:red 12 :green 14 :blue 14}]
                                    3 [{:blue 3 :red 4} {:red 9 :green 7}]})))

  (it "finds minimum-set-of-cubes"
    (should= {} (get-minimum-color-set [{}]))
    (should= {:red 12 :green 13 :blue 14}
             (get-minimum-color-set [{:red 12 :green 13 :blue 14}]))
    )
  )

(describe "solutions 1"
  (it "solves test data"
    (should= 8 (sum-valid-ids "test-data")))

  (it "solves problem 1"
    (should= 2679 (sum-valid-ids "input"))))

(describe "solutions 2"
  (it "solves test data"
    (should= 2286 (sum-powers "test-data")))

  (it "solves problem 2"
    (should= 77607 (sum-powers "input"))))
