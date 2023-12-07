(ns aoc7.core-spec
  (:require [aoc7.core :refer :all]
            [speclj.core :refer :all]))

(describe "parsing hands"
  (it "parses the file"
    (should= [["32T3K" 765]
              ["T55J5" 684]
              ["KK677" 28]
              ["KTJJT" 220]
              ["QQQJA" 483]] (parse-hands "test-data"))))

(describe "hand evaluations"
  (it "decodes the type of the hand"
    (should= 1 (type-of-hand "12345"))
    (should= 2 (type-of-hand "AA123"))
    (should= 3 (type-of-hand "AAKK1"))
    (should= 4 (type-of-hand "AAA12"))
    (should= 5 (type-of-hand "AAAKK"))
    (should= 6 (type-of-hand "AAAAK"))
    (should= 7 (type-of-hand "AAAAA")))

  (it "decodes type of hand with joker"
    (should= 2 (type-of-hand "A*123"))
    (should= 4 (type-of-hand "AA*12"))
    (should= 4 (type-of-hand "A**12"))
    (should= 5 (type-of-hand "AA*KK"))
    (should= 6 (type-of-hand "A**KK"))
    (should= 6 (type-of-hand "AAA*K"))
    (should= 6 (type-of-hand "AK***"))
    (should= 7 (type-of-hand "AAAA*"))
    (should= 7 (type-of-hand "AAA**"))
    (should= 7 (type-of-hand "AA***"))
    (should= 7 (type-of-hand "A****"))
    (should= 7 (type-of-hand "*****"))

    )

  (it "knows what cards are higher"
    (should (greater-card \A \K))
    (should (greater-card \K \Q))
    (should (greater-card \Q \J))
    (should (greater-card \J \T))
    (should (greater-card \T \9))
    (should (greater-card \9 \8))
    (should (greater-card \8 \7))
    (should (greater-card \7 \6))
    (should (greater-card \6 \5))
    (should (greater-card \5 \4))
    (should (greater-card \4 \3))
    (should (greater-card \3 \2)))

  (it "knows what hand is greater"
    (should (greater-hand "AAAAA" "AAAA1"))
    (should (greater-hand "AAAA1" "AAA11"))
    (should (greater-hand "AAA11" "AAA12"))
    (should (greater-hand "AAA12" "AA1KK"))
    (should (greater-hand "AAKK1" "AA123"))
    (should (greater-hand "AA123" "12345"))
    (should (greater-hand "3AKQJ" "2AKQJ"))
    (should (greater-hand "A3KQJ" "A2KQJ"))
    (should (greater-hand "AK3QJ" "AK2QJ"))
    (should (greater-hand "AKQ3J" "AKQ2J"))
    (should (greater-hand "AKQJ3" "AKQJ2"))
    (should-not (greater-hand "23456" "23456"))
    (should (greater-hand "KK677" "KTJJT"))
    )

  (it "sorts hands with bids"
    (should= [["AAAKK" 11] ["AAAAA" 10]]
             (sort-hands-with-bids [["AAAAA" 10] ["AAAKK" 11]]))
    (should= [["AAAKK" 11] ["AAAAA" 10]]
             (sort-hands-with-bids [["AAAKK" 11] ["AAAAA" 10]]))))

(describe "solution 1"
  (it "solves part 1"
    (should= 6440 (product-of-bids-by-rank "test-data"))
    (should= 248217452 (product-of-bids-by-rank "input"))))

(describe "jokers"
  (it "converts J to *"
    (should= "A1*23" (jokerize "A1J23"))))

(describe "solution 2"
  (it "solves part 2"
    (should= 5905 (product-of-bids-by-rank-with-jokers "test-data"))
    (should= 245576185 (product-of-bids-by-rank-with-jokers "input"))))