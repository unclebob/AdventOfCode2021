(ns day11.core-spec
  (:require [speclj.core :refer :all]
            [day11.core :refer :all]))

(describe "parsing the input"
  (it "parses a line"
    (should= [1 2 3 4 5] (parse-line "12345")))

  (it "parses many lines"
    (should= [[1 2 3 4] [5 6 7 8]]
             (parse-lines "1234\n5678\n")))

  (it "creates the coordinate map"
    (should= {[0 0] 1
              [1 0] 2
              [2 0] 3
              [0 1] 4
              [1 1] 5
              [2 1] 6} (parse-input "123\n456\n"))))

(def world3x3 (grid-to-map [[1 1 1]
                            [1 1 1]
                            [1 1 1]]))

(describe "utilities"
  (context "diagonals"
    (it "calculates diagonals in the middle"
      (should= #{[0 0] [1 0] [2 0]
                 [0 1] [2 1]
                 [0 2] [1 2] [2 2]} (get-diagonals world3x3 [1 1])))

    (it "will get diagonals of the corners and edges"
      (should= #{[1 1] [2 1] [1 2]} (get-diagonals world3x3 [2 2]))
      (should= #{[1 0] [0 1] [1 1]} (get-diagonals world3x3 [0 0]))
      (should= #{[0 0] [1 0] [1 1] [0 2] [1 2]} (get-diagonals world3x3 [0 1]))))

  (context "flashing"
    (it "increments energy"
      (should= {[0 0] 1 [1 0] 2}
               (increment-energy {[0 0] 0 [1 0] 1})))

    (it "one flasher"
      (should= [1 (grid-to-map [[2 2 2]
                                [2 0 2]
                                [2 2 2]])]
               (flash (grid-to-map [[1 1 1]
                                    [1 10 1]
                                    [1 1 1]]))))

    (it "two flashers"
      (should= [2 (grid-to-map [[2 3 3]
                                [2 0 0]
                                [2 3 3]])]
               (flash (grid-to-map [[1 1 1]
                                    [1 10 10]
                                    [1 1 1]]))))

    (it "cascades flashes"
      (should= [3 (grid-to-map [[2 3 3]
                                [2 0 0]
                                [2 4 0]])]
               (flash (grid-to-map [[1 1 1]
                                    [1 10 10]
                                    [1 1 8]])))))
  )

(def test-data "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526")

(describe "solutions"
  (context "part 1"
    (it "solves sample"
      (should= 1656 (solve-1 (parse-input test-data))))

    (it "solves problem"
      (should= 1723 (solve-1 (parse-input (slurp "input")))))

    )

  (context "part 2"
    (it "solves sample"
      (should= 195 (solve-2 (parse-input test-data))))

    (it "solves problem"
      (should= 327 (solve-2 (parse-input (slurp "input"))))))
  )

