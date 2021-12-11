(ns day9.core-spec
  (:require [speclj.core :refer :all]
            [day9.core :refer :all]))

(describe "parsing input"
  (it "can parse a line"
    (should= [1 2 3 4 5] (parse-line "12345")))

  (it "can parse many lines"
    (should= [[1 2 3 4] [5 6 7 8]]
             (parse-input "1234\n5678\n")))
  )

(def sample-data "2199943210\n3987894921\n9856789892\n8767896789\n9899965678")

(describe "utilitiies"
  (let [data (parse-input "1234\n5678")]
    (it "can use coordinates to access the data"
      (should= 1 (get-height data [0 0]))
      (should= 7 (get-height data [2 1]))
      (should= 8 (get-height data [3 1])))

    (it "makes out-of-bounds coordinates max height"
      (should= 10 (get-height data [-1 -1]))
      (should= 10 (get-height data [0 -1]))
      (should= 10 (get-height data [-1 0]))
      (should= 10 (get-height data [99 0]))
      (should= 10 (get-height data [0 99]))))

  (let [data [[3 3 3]
              [3 1 3]
              [3 3 3]]]
    (it "will decide low point"
      (should (low? data [1 1]))
      (should-not (low? data [0 0]))
      (should-not (low? data [2 2])))

    (it "will find low points"
      (should= #{[1 1]} (find-lows data))))

  (context "basins"
    (let [data [[9 9 9 9]
                [9 9 2 9]
                [1 2 0 2]
                [9 2 2 9]]]
      (it "will find basin points"
        (should= #{[2 2]
                   [1 2]
                   [2 1]
                   [3 2]
                   [2 3]
                   [0 2]
                   [1 3]}
                 (find-basin-points data [2 2])))))
  )

(describe "solutions"
  (context "part 1"
    (it "solves sample"
      (should= 15 (solve-1 sample-data)))
    (it "solves problem"
      (should= 575 (solve-1 (slurp "input"))))
    )

  (context "part 2"
    (it "solves sample"
      (should= 1134 (solve-2 sample-data)))

    (it "solves problem"
      (should= 1019700 (solve-2 (slurp "input"))))))
