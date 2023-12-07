(ns aoc1a.core-spec
  (:require [speclj.core :refer :all]
            [aoc1a.core :refer :all]))

(describe "calibration value"
  (it "unspells digits"
    (should= "0ero" (unspell-digits "zero"))
    (should= "a1neb2woc" (unspell-digits "aonebtwoc"))
    )

  (it "filters digits"
    (should= ["1" "2"] (filter-digits "1abc2")))

  (it "is the concatenation of the first and last digit"
    (should= 12 (get-calibration-value "1abc2"))
    (should= 38 (get-calibration-value "pqr3stu8vwx"))
    (should= 15 (get-calibration-value "a1b2c3d4e5f"))
    (should= 77 (get-calibration-value "treb7uchet"))
    (should= 29 (get-calibration-value "two1nine")))

  (it "sums all the calibrations"
    (should= 142 (sum-calibrations "test-data"))
    ;(should= 55447 (sum-calibrations "input"))
    (should= 54706 (sum-calibrations "input")))

  (it "sums all the calibrations"
    (should= 281 (sum-calibrations "test-data-2")))
  )


