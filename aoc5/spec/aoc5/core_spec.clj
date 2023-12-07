(ns aoc5.core-spec
  (:require [speclj.core :refer :all]
            [aoc5.core :refer :all]))

(describe "parsing"
  (it "parses the almanac"
    (should= {:seeds [1 2 3]
              :x [[1 2 3] [4 5 6]]
              :y [[7 8 9]]}
             (parse-almanac ["seeds: 1 2 3"
                             "x map:"
                             "1 2 3"
                             "4 5 6"
                             ""
                             "y map:"
                             "7 8 9"]))))

(describe "mapping"
  (it "maps a number to a range"
    (should= 2 (map-to-range 2 [13 3 3]))
    (should= 13 (map-to-range 3 [13 3 3]))
    (should= 14 (map-to-range 4 [13 3 3]))
    (should= 6 (map-to-range 6 [13 3 3])))

  (it "maps to ranges"
    (should= 2 (map-to-ranges 2 [[13 3 3]]))
    (should= 34 (map-to-ranges 10 [[13 3 3] [33 9 5]])))

  (it "maps sources to ranges"
    (should= [2 34] (map-sources-to-ranges [2 10] [[13 3 3] [33 9 5]])))

  (it "maps a source range to a destination range"
    (should= [[] [[3 2]]] (map-source-range-to-dest [3 2] [20 10 2]))
    (should= [[] [[12 2]]] (map-source-range-to-dest [12 2] [20 10 2]))
    (should= [[20 1] [[9 1]]] (map-source-range-to-dest [9 2] [20 10 5]))
    (should= [[20 2] [[8 2]]] (map-source-range-to-dest [8 4] [20 10 5]))
    (should= [[21 2] []] (map-source-range-to-dest [11 2] [20 10 5]))
    (should= [[23 2] [[15 2]]] (map-source-range-to-dest [13 4] [20 10 5]))
    (should= [[20 5] [[8 2] [15 3]]] (map-source-range-to-dest [8 10] [20 10 5])))

  (it "maps source ranges to a destination range"
    (should= [[] [[3 2] [12 2]]]
             (map-source-ranges-to-dest [[3 2] [12 2]] [20 10 2]))
    (should= [[[23 2] [20 5]] [[15 2] [8 2] [15 3]]]
             (map-source-ranges-to-dest [[13 4] [8 10]] [20 10 5]))
    (should= [[[100 2] [103 2]] [[1 2] [19 1] [25 2] [35 3]]]
             (map-source-ranges-to-dest
               [[1 2] [19 3] [23 4] [35 3]]
               [100 20 5]))
    )

  (it "maps source ranges to all destination ranges"
    (should= #{[100 2] [103 2] [35 3] [19 1] [200 1] [45 2] [25 2] [1 2] [39 1]}
             (map-source-ranges-to-all-dests
               [[1 2] [19 3] [23 4] [35 3] [39 2] [45 2]]
               [[100 20 5] [200 40 3]])))
  )

(describe "solutions"
  (it "solves 1 test-data"
    (should= 35 (find-nearest-location "test-data")))

  (it "solves 1 input"
    (should= 836040384 (find-nearest-location "input")))

  (it "solves 2 test-data"
    (should= 46 (find-nearest-location-using-seed-ranges "test-data")))

  (it "solves 2 input"
    (should= 10834440 (find-nearest-location-using-seed-ranges "input")))
  )


