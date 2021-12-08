(ns day5.core-spec
  (:require [speclj.core :refer :all]
            [day5.core :refer :all]))
(def sample "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2\n")

(describe "parsing input"
  (it "should parse one line"
    (should= [[1 2] [3 4]]
             (parse-line "1,2 -> 3,4\n")))

  (it "should parse many lines"
    (should= [[[1 2] [3 4]] [[5 6] [7 8]]]
             (parse-lines "1,2 -> 3,4\n5,6 -> 7,8\n")))
  )

(describe "utilities"
  (it "should find the unit vector"
    (should= [0 0] (unit-vector [[1 1] [1 1]]))
    (should= [1 1] (unit-vector [[1 2] [5 6]]))
    (should= [-1 -1] (unit-vector [[5 6] [1 2]]))
    (should= [1 0] (unit-vector [[1 2] [5 2]]))
    (should= [0 -1] (unit-vector [[5 6] [5 2]]))
    )

  (it "should add vectors to points"
    (should= [3 4] (add-vector [1 1] [2 3])))

  (it "should find points on line"
    (should= #{[1 1] [2 2] [3 3]}
             (points-on-line [[1 1] [3 3]]))
    (should= #{[1 1] [2 2] [3 3]}
             (points-on-line [[3 3] [1 1]]))
    (should= #{[1 1]}
             (points-on-line [[1 1] [1 1]]))
    )

  (it "should accumulate points"
    (should= {[1 1] 1}
             (accumulate-points [#{[1 1]}]))
    (should= {[1 1] 2 [2 2] 2 [3 3] 1}
             (accumulate-points [#{[1 1] [2 2]}
                                 #{[1 1] [2 2] [3 3]}]))

    )

  (it "should solve sample part 1"
    (should= 5 (solve-1 sample)))

  (it "should solve part 1"
    (should= 7269 (solve-1 (slurp "input"))))

  (it "should solve sample part 2"
    (should= 12 (solve-2 sample)))

  (it "should solve part 2"
    (should= 21140 (solve-2 (slurp "input"))))
  )
