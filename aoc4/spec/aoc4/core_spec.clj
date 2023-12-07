(ns aoc4.core-spec
  (:require [speclj.core :refer :all]
            [aoc4.core :refer :all]))

(describe "cards"
  (it "parses a card"
    (should= [1 [#{} #{}]] (parse-card "Card 1: | "))
    (should= [999 [#{1 2 3} #{15 16 17}]] (parse-card "Card   999: 1  2  3  |  15  16  17 "))
    )

  (it "parses the cards"
    (should= {} (parse-cards []))
    (should= {1 [#{} #{}]} (parse-cards ["Card 1: | "]))
    (should= {1 [#{1 2} #{3 4}]
              2 [#{5 6} #{7 8}]}
             (parse-cards ["Card 1: 1 2 | 3 4"
                           "Card 2: 5 6 | 7 8"]))
    )

  (it "values the cards"
    (should= 0 (card-value [#{1 2} #{3 4}]))
    (should= 1 (card-value [#{1 2} #{1 4}]))
    (should= 2 (card-value [#{1 2} #{1 2}]))
    (should= 4 (card-value [#{1 2 3} #{3 1 2}])))

  (it "counts the matches"
    (should= 0 (card-matches [#{1 2} #{3 4}]))
    (should= 1 (card-matches [#{1 2} #{1 4}]))
    (should= 2 (card-matches [#{1 2} #{1 2}]))
    (should= 3 (card-matches [#{1 2 3} #{3 1 2}])))
)

(describe "solution 1"
  (it "solves 1 test-data"
    (should= 13 (sum-card-values "test-data")))

  (it "solves 1 input"
    (should= 22488 (sum-card-values "input"))))

(describe "counting cards"
  (it "should make a list of card counts"
    (should= {1 1 2 1 3 1} (make-counts {1 nil 2 nil 3 nil})))

  (it "should update counts"
    (should= {1 1 2 10 3 10} (update-counts {1 1 2 1 3 1} 1 2 9))))

(describe "solution 2"
  (it "solves 2 test-data"
    (should= 30 (count-all-cards "test-data")))

  (it "solves 2 input"
    (should= 7013204 (count-all-cards "input"))))
