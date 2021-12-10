(ns day8.core-spec
  (:require [speclj.core :refer :all]
            [day8.core :refer :all]))

(describe "parse input"
  (it "parses a line"
    (should= [["acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab"]
              ["cdfeb" "fcadb" "cdfeb" "cdbaf"]]
             (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
    cdfeb fcadb cdfeb cdbaf")))

  (it "parses several lines"
    (should= [
              [["be" "cfbegad" "cbdgef" "fgaecd" "cgeb" "fdcge" "agebfd" "fecdb" "fabcd" "edb"]
               ["fdgacbe" "cefdb" "cefbgd" "gcbe"]]
              [["edbfga" "begcd" "cbg" "gc" "gcadebf" "fbgde" "acbgfd" "abcde" "gfcbed" "gfec"]
               ["fcgedb" "cgb" "dgebacf" "gc"]]]
             (parse-input (str "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n"
                               "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n")))

    )
  )

(def test-data [["acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab"]
              ["cdfeb" "fcadb" "cdfeb" "cdbaf"]])

(def test-digits (first test-data))

(describe "utilities"
  (it "counts unique digits in empty display"
    (should= 0 (count-unique [])))
  (it "counts 1, 4, 7, and 8"
    (should= 4 (count-unique ["11" "4444" "777" "8888888" "22222"]))
    )
  (it "should translate counts to digits"
    (should= 1 (to-digit 2))
    (should= 4 (to-digit 4))
    (should= 7 (to-digit 3))
    (should= 8 (to-digit 7))
    (should= nil (to-digit 1)))

  (it "should propose segments"
    (should= nil (propose-outputs-for-signals "abcdef"))
    (should= [[\q #{\C \F}]
              [\r #{\C \F}]]
             (propose-outputs-for-signals "qr")))

  (it "should determine possible outputs"
    (should= {\a #{\C \F}
              \b #{\C \F}
              \c #{\A \B \C \D \E \F \G}
              \d #{\A \C \F}
              \e #{\B \C \D \F}
              \f #{\B \C \D \F}
              \g #{\A \B \C \D \E \F \G}}
             (possible-outputs test-digits)))

  (it "should determine digit signal map"
    (should= {4 #{\a \b \e \f},
              1 #{\a \b},
              7 #{\a \b \d}} (signals-for-obvious-digits test-digits)))

  (it "will find signals for digits of length n"
    (should= [#{\a \b \d}] (signals-for-length 3 test-digits))
    (should= [#{\a \b \c \d \e \f}
               #{\b \c \d \e \f \g}
               #{\a \b \c \d \e \g}] (signals-for-length 6 test-digits)))

  (it "will map signals to outputs"
    (should= {\d \A, \a \C, \b \F, \c \G, \f \D, \e \B, \g \E} (map-signals-to-outputs test-digits)))

  (it "will map output sets to digits"
    (let [signal-map (map-signals-to-outputs test-digits)]
      (should= 0 (outputs-to-digits signal-map "abcged"))
      (should= 1 (outputs-to-digits signal-map "ab"))
      (should= 2 (outputs-to-digits signal-map "dafgc"))
      (should= 3 (outputs-to-digits signal-map "dafbc"))
      ))

  (it "will translate a display"
    (should= 5353 (translate-display test-data)))
  )

(def sample (str "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n"
                 "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n"
                 "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n"
                 "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n"
                 "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n"
                 "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n"
                 "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n"
                 "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n"
                 "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n"
                 "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce\n"))

(describe "solutions"
  (context "part 1"
    (it "should solve sample"
      (should= 26 (solve-1 sample)))

    (it "should solve problem"
      (should= 390 (solve-1 (slurp "input"))))

    )

  (context "part 2"
    (it "should solve sample"
      (should= 61229 (solve-2 sample)))

    (it "should solve problem"
      (should= 1011785 (solve-2 (slurp "input"))))

    )
  )
