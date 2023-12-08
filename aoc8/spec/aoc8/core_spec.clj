(ns aoc8.core-spec
  (:require [aoc8.core :refer :all]
            [speclj.core :refer :all]))

(describe "parsing input"
  (it "parses the map"
    (should= ["LLR" {"AAA" ["BBB" "BBB"]
                     "BBB" ["AAA" "ZZZ"]
                     "ZZZ" ["ZZZ" "ZZZ"]}]
             (parse-map "test-data"))))

(describe "solve part 1"
  (it "solves test data"
    (should= 6 (count-steps "test-data")))
  (it "solves input"
    (should= 22411 (count-steps "input"))))

(def test-map-2 (second (parse-map "test-data-2")))
(prn 'test-map-2 test-map-2)

(describe "counting terminating paths"
  (it "should count the paths from a starting point"
    (should= #{1} (count-paths {"11A" ["11Z" "11Z"]} "11A" "R"))
    (should= #{2} (count-paths {"11A" ["11B" "11B"]
                                "11B" ["11Z" "11Z"]} "11A" "R"))
    (should= #{2 3} (count-paths {"11A" ["11B" "11B"]
                                  "11B" ["11Z" "11Z"]
                                  "11Z" ["22Z" "22Z"]
                                  "22Z" ["11A" "11A"]} "11A" "R"))
    (should= #{2} (count-paths test-map-2 "11A" "LR"))
    (should= #{3} (count-paths test-map-2 "22A" "LR"))

    ))

(describe "solve part 2"
  (it "solves test data 2"
    (should= 6 (count-concurrent-steps "test-data-2")))

  (it "solves input"
    (should= 11188774513823 (count-concurrent-steps "input")))
  )
