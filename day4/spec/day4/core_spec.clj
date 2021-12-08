(ns day4.core-spec
  (:require [speclj.core :refer :all]
            [day4.core :refer :all]))

(describe
  "parsing input"
  (it "should parse a sample"
    (should= [[1 2 3 4 5]
              [[[1 2 3 4 5]
                [1 2 3 4 5]
                [1 2 3 4 5]
                [1 2 3 4 5]
                [1 2 3 4 5]]
               [[2 3 4 5 6]
                [2 3 4 5 6]
                [2 3 4 5 6]
                [2 3 4 5 6]
                [2 3 4 5 6]]]]
             (parse-input (str "1,2,3,4,5\n"
                               "\n"
                               "1 2 3 4 5\n"
                               "1 2 3 4 5\n"
                               "1 2 3 4 5\n"
                               "1 2 3 4 5\n"
                               "1 2 3 4 5\n"
                               "\n"
                               "2 3 4 5 6\n"
                               "2 3 4 5 6\n"
                               "2 3 4 5 6\n"
                               "2 3 4 5 6\n"
                               "2 3 4 5 6\n")))))

(describe "The tools"
  (it "Should mark deleted items"
    (should= [[1 2 "X" 4 5]
              [6 7 8 9 10]]
             (play-board 3 [[1 2 3 4 5]
                            [6 7 8 9 10]])))

  (it "should win horizontally"
    (should (win? [["X" "X" "X" "X" "X"]
                   [0 0 0 0 0]
                   [0 0 0 0 0]
                   [0 0 0 0 0]
                   [0 0 0 0 0]]))
    (should (win? [[0 0 0 0 0]
                   ["X" "X" "X" "X" "X"]
                   [0 0 0 0 0]
                   [0 0 0 0 0]
                   [0 0 0 0 0]]))
    (should (win? [[0 "X" 0 0 0]
                   [0 "X" 0 0 0]
                   [0 "X" 0 0 0]
                   [0 "X" 0 0 0]
                   [0 "X" 0 0 0]])))

  (it "scores boards"
    (should= 6 (score 2 [[0 "X" 0 0 0]
                         [1 "X" 0 0 0]
                         [0 "X" 0 2 0]
                         [0 "X" 0 0 0]
                         [0 "X" 0 0 0]])))
  )


(def sample-input-text "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7")
(describe "Sample Input"
  (it "should parse"
    (let [[draws boards] (parse-input sample-input-text)]
      (should= [7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1]
               draws)
      (should= [22 13 17 11 0] (first (first boards)))
    ))

  (it "should solve 1 for sample input"
    (let [[draws boards] (parse-input sample-input-text)
          score (bingo-winner draws boards)]
      (should= 4512 score)))

  (it "should solve 2 for sample input"
    (let [[draws boards] (parse-input sample-input-text)
          score (bingo-loser draws boards)]
      (should= 1924 (first (first score)))))
  )

(describe
  "The actual problem"
  (let [[draws boards] (parse-input (slurp "input"))]
    (it "should solve 1"
      (should= 55770 (bingo-winner draws boards)))

    (it "should solve 2"
      (should= 2980 (first (first (bingo-loser draws boards)))))
    ))


