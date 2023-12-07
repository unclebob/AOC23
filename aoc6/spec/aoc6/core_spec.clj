(ns aoc6.core-spec
  (:require [aoc6.core :refer :all]
            [speclj.core :refer :all]))

(describe "parsing"
  (it "parses races"
    (should= [[7 9] [15 40] [30 200]]
             (parse-races ["Time:      7  15   30"
                           "Distance:  9  40  200"])))

  (it "parses races test-data file"
    (should= [[7 9] [15 40] [30 200]]
             (parse-races-file "test-data"))))

(describe "races"
  (it "calculates distance of race"
    (should= 0 (race 7 0))
    (should= 6 (race 7 1))
    (should= 10 (race 7 2))
    (should= 0 (race 7 7))
    (should= 0 (race 15 0))
    (should= 14 (race 15 1))
    )

  (it "calculates distances of races"
    (should= [0 6 10 12 12 10 6 0] (races 7))
    (should= [0 14 26 36 44 50 54 56 56 54 50 44 36 26 14 0] (races 15)))

  (it "finds winners"
    (should= 4 (winners 7 9))
    (should= [4 8 9] (all-winners [[7 9] [15 40] [30 200]]))))

(describe "solution 1"
  (it "solves part 1"
    (should= 288 (product-of-all-winners "test-data"))
    (should= 633080 (product-of-all-winners "input"))))

(describe "parse bad kerning"
  (it "eliminates the spaces"
    (should= [71530 940200] (parse-badly-kerned-race "test-data"))))

(describe "solution 2"
  (it "solves part 2"
    (should= 71503 (apply winners (parse-badly-kerned-race "test-data")))
    (should= 20048741 (apply winners (parse-badly-kerned-race "input")))))

