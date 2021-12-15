(ns day12.core-spec
  (:require [speclj.core :refer :all]
            [day12.core :refer :all]))


(describe "parsing the input"
  (it "parses a line"
    (should= ["ALPHA" "beta"] (parse-line "ALPHA-beta")))

  (it "parses many lines"
    (should= [["a" "b"] ["b" "c"]]
             (parse-lines "a-b\nb-c")))

  (it "builds the map of paths"
    (should= {"a" #{"b"}
              "b" #{"c" "a"}
              "c" #{"b"}}
             (parse-input "a-b\nb-c"))
    )

  (it "does not include start in the map"
    (should= {"start" #{"end"}
              "end" #{}}
             (parse-input "start-end\n")))

  (it "parses small test data"
    (should= {"start" #{"b" "A"},
              "A" #{"b" "end" "c"},
              "b" #{"d" "A" "end"},
              "c" #{"A"},
              "d" #{"b"},
              "end" #{"b" "A"}}
             (parse-input (slurp "small-test-data"))))
  )

(defn fp [input]
  (find-paths (parse-input input)))

(defn fpc [input special-cave]
  (find-paths (parse-input input) special-cave))

(describe "utilities"
  (it "identifies small and large"
    (should (small? "a"))
    (should (small? "start"))
    (should-not (small? "A")))

  (context "finding paths"
    (it "finds no path if no start or end"
      (should-be empty? (fp "a-b"))
      (should-be empty? (fp "start-a"))
      (should-be empty? (fp "b-end")))

    (it "finds one path from start-end"
      (should= [["start" "end"]] (fp "start-end"))
      (should= [["start" "end"]] (fp "start-a\nstart-end")))

    (it "finds two paths"
      (should= [["start" "a" "end"]
                ["start" "b" "end"]]
               (fp (str "start-a\n"
                        "start-b\n"
                        "a-end\n"
                        "b-end\n"))))

    (it "finds the paths with a special cave"
      (should=
        [["start" "a" "b" "end"]
         ["start" "a" "b" "c" "b" "end"]
         ["start" "a" "b" "c" "end"]
         ["start" "a" "c" "b" "end"]
         ["start" "a" "c" "end"]]
        (fpc "start-a\na-b\na-c\nb-c\nb-end\nc-end" "b")))

    (it "finds the paths in the small test data"
      (should= #{"start,A,b,A,c,A,end"
                 "start,A,b,A,end"
                 "start,A,b,end"
                 "start,A,c,A,b,A,end"
                 "start,A,c,A,b,end"
                 "start,A,c,A,end"
                 "start,A,end"
                 "start,b,A,c,A,end"
                 "start,b,A,end"
                 "start,b,end"}
               (format-paths (fp (slurp "small-test-data")))))

    (it "counts the paths in the medium test data"
      (should= 19 (count (fp (slurp "medium-test-data")))))

    (it "counts the paths in the large test data"
      (should= 226 (count (fp (slurp "large-test-data")))))
    )
  )


(describe "solutions"
  (context "part 1"
    (it "should solve part 1"
      (should= 3495 (count (fp (slurp "input"))))))

  (context "part 2"
    (it "should solve the small test data"
      (should= 36 (solve-2 (parse-input (slurp "small-test-data")))))

    (it "should solve the medium test data"
      (should= 103 (solve-2 (parse-input (slurp "medium-test-data")))))

    (it "should solve the large test data"
      (should= 3509 (solve-2 (parse-input (slurp "large-test-data")))))

    (it "should solve the problem"
      (should= 94849 (solve-2 (parse-input (slurp "input")))))
    )

  )
