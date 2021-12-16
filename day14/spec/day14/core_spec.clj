(ns day14.core-spec
  (:require [speclj.core :refer :all]
            [day14.core :refer :all]))

(describe "parsing"
  (it "parses a line"
    (should= [:template "NNCB"]
             (parse-line "NNCB"))
    (should= [:template "ABCDE"]
             (parse-line "ABCDE"))
    (should= [:insert ["NN" "C"]]
             (parse-line "NN -> C")))

  (it "parses many lines"
    (should= {:template "NNBC"
              :insert {"NN" "C"
                       "BB" "D"}}
             (parse-input "NNBC\n\nNN -> C\nBB -> D\n")))

  )

(describe "utilities"
  (it "polymerizes"
    (should= "ACB"
             (polymerize {"AB" "C"}
                         "AB"))
    (should= "ADBEC"
             (polymerize {"AB" "D"
                          "BC" "E"}
                         "ABC")))

  (it "counts dimers"
    (should= {}
             (count-dimers ""))
    (should= {}
             (count-dimers "A"))
    (should= {"AB" 1}
             (count-dimers "AB"))
    (should= {"AB" 1
              "BC" 1}
             (count-dimers "ABC"))
    (should= {"AB" 2
              "BC" 2
              "CA" 1}
             (count-dimers "ABCABC")))

  (it "count insertions"
    (should= {}
             (count-insertions {} {}))

    (should= {"AB" 1}
             (count-insertions {} {"AB" 1}))

    (should= {}
             (count-insertions {"AB" "C"} {}))

    (should= {"AC" 1
              "CB" 1}
             (count-insertions {"AB" "C"}
                               {"AB" 1}))

    (should= {"AD" 2
              "DB" 1
              "DC" 1}
             (count-insertions {"AB" "D"
                                "AC" "D"}
                               {"AB" 1
                                "AC" 1}))

    )

  (context "sample data"
    (let [{:keys [template insert]} (parse-input (slurp "sample"))
          iteration (iterate (partial polymerize insert) template)]
      (it "polymerizes"
        (should= "NCNBCHB" (nth iteration 1))
        (should= "NBCCNBBBCBHCB" (nth iteration 2))
        (should= "NBBBCNCCNBBNBNBBCHBHHBCHB" (nth iteration 3))))
    )
  )

(describe "solutions"
  (context "part 1"
    (it "solves sample"
      (should= 1588 (solve-1 (slurp "sample"))))

    (it "solves problem"
      (should= 3697 (solve-1 (slurp "input"))))
    )

  (context "part 2"
    (it "solves sample for 10"
      (should= 1588 (solve-2 10 (slurp "sample"))))

    (it "solves sample for 40"
      (should= 2188189693529 (solve-2 40 (slurp "sample"))))

    (it "solves problem"
      (should= 4371307836157 (solve-2 40 (slurp "input"))))
    )
  )
