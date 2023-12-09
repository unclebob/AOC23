(ns aoc9.core-spec
  (:require [aoc9.core :refer :all]
            [speclj.core :refer :all]))

(describe "parsing the histories"
  (it "parses one line"
    (should= [1 2 3] (parse-history-line "1 2 3")))

  (it "parses many lines"
    (should= [[1 2 3] [4 5 6]] (parse-history-lines ["1 2 3" "4 5 6"])))

  (it "parses test data file"
    (should= [[0 3 6 9 12 15]
              [1 3 6 10 15 21]
              [10 13 16 21 30 45]] (parse-history-file "test-data"))))

(describe "the method of Finite Differences"
  (it "differentiates"
    (should= [3 3 3 3 3] (differentiate [0 3 6 9 12 15])))

  (it "creates table of differences"
    (should= [[0 3 6 9 12 15] [3 3 3 3 3]]
             (make-difference-table [0 3 6 9 12 15])))

  (it "extrapolates next value"
    (should= 18 (next-value [0 3 6 9 12 15])))

  (it "extrapolates previous value"
    (should= 5 (previous-value [10 13 16 21 30 45]))
    (should= 0 (previous-value [1 3 6 10 15 21]))
    (should= -3 (previous-value [0 3 6 9 12 15]))
    )
  )

(describe "solves part 1"
  (it "solves test data"
    (should= 114 (sum-extrapolations "test-data")))

  (it "solves input"
    (should= 1641934234 (sum-extrapolations "input"))))

(describe "solves part 2"
  (it "solves test-data"
    (should= 2 (sum-predictions "test-data")))

  (it "solves input"
      (should= 975 (sum-predictions "input"))))
