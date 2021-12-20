(ns day17.core-spec
  (:require [speclj.core :refer :all]
            [day17.core :refer :all]))

(let [sample "target area: x=20..30, y=-10..-5"
      box [20 30 -10 -5]]
  (describe "parsing input"
    (it "parses"
      (should= box
               (parse-input sample))))

  (describe "utilities"
    (context "in?"
      (it "outside"
        (should-not (in? box [0 0]))
        (should-not (in? box [19 -6]))
        (should-not (in? box [31 -6]))
        (should-not (in? box [15 -11]))
        (should-not (in? box [15 -4])))

      (it "inside"
        (should (in? box [20 -10]))
        (should (in? box [30 -10]))
        (should (in? box [20 -5]))
        (should (in? box [25 -7])))
      )

    (context "beyond?"
      (it "isn't"
        (should-not (beyond? box [0 0]))
        (should-not (beyond? box [25 -7])))

      (it "is"
        (should (beyond? box [31 -7]))
        (should (beyond? box [25 -11]))))

    (context "limits"
      (it "max-vx"
        (should= 31 (max-vx box)))

      (it "min-vx"
        (should= 5 (min-vx box)))

      (it "min-vy"
        (should= 0 (min-vy box)))

      (it "max-vy"
        (should= 11 (max-vy box)))
      )

    (context "shot"
      (it "misses"
        (should= :miss (shot box [0 1]))
        (should= :miss (shot box [21 5]))
        (should= :miss (shot box [4 5]))
        (should= :miss (shot box [6 12]))
        (should= :miss (shot box [17 -4]))
        )

      (it "hits"
        (should= 3 (shot box [7 2]))
        (should= 6 (shot box [6 3]))
        (should= 0 (shot box [9 0]))
        )
      )
    )

  (describe "solution"
    (context "part 1"
      (it "solves sample"
        (should= 45 (solve-1 (slurp "sample"))))

      (it "solves problem"
        (should= 6555 (solve-1 (slurp "input")))))

    (context "part 2"
      (it "solves sample"
        (should= 112 (solve-2 (slurp "sample"))))

      (it "solves problem"
        (should= 4973 (solve-2 (slurp "input"))))))
  )
