(ns day20.core-spec
  (:require [speclj.core :refer :all]
            [day20.core :refer :all]))

(describe "parsing"
  (it "parses initial image"
    (should= {[0 0] \.
              [1 0] \#
              [2 0] \.
              [0 1] \#
              [1 1] \.
              [2 1] \#}
             (parse-image [".#."
                           "#.#"]))))


(describe "utilities"
  (it "finds bounding rectangle"
    (should= [[0 0] [2 1]]
             (bounds (parse-image [".#."
                                   "#.#"]))))

  (it "expands bounds"
    (should= [[-1 -1] [3 2]]
             (expand-box (bounds (parse-image [".#."
                                               "#.#"])))))

  (it "computes neighbors"
    (should= [[-1 -1] [0 -1] [1 -1]
              [-1 0] [0 0] [1 0]
              [-1 1] [0 1] [1 1]]
             (neighbors-of [0 0])))

  (it "finds the binary index"
    (let [image (parse-image [".#."
                              "#.#"])]
      (should= 168 (binary-index-of image [1 1]))))

  (it "finds the binary index from sample"
    (let [image (parse-image ["#..#."
                              "#...."
                              "##..#"
                              "..#.."
                              "..###"])]
      (should= 1 (binary-index-of image [-1 -1]))
      (should= 9 (binary-index-of image [-1 0]))
      (should= 73 (binary-index-of image [-1 1])))))

(describe "solutions"
  (context "part 1"
    (it "solves sample"
      (should= 35 (solve (slurp "sample") 2)))

    (it "solves problem"
      (should= 5065 (solve (slurp "input") 2))))

  (context "part 2"
    (it "solves sample"
      (should= 3351 (solve (slurp "sample") 50)))

    (it "solves problem"
      (should= 14790 (solve (slurp "input") 50)))))

