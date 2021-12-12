(ns day10.core-spec
  (:require [speclj.core :refer :all]
            [day10.core :refer :all]))

(describe "parses input"
  (it "simply breaks into lines"
    (should= ["abc" "def"] (parse-input "abc\ndef\n")))
  )

(describe "utilities"
  (it "empty string is a chunk"
    (should= "" (parse-chunk "")))

  (it "knows how to close an opening"
    (should= \) (closer-for \())
    (should= \] (closer-for \[))
    (should= \} (closer-for \{))
    (should= \> (closer-for \<))
    )

  (it "knows openers"
    (should (opener? \())
    (should (opener? \[))
    (should (opener? \{))
    (should (opener? \<))
    )

  (it "validates simple pairs"
    (should= "" (parse-chunk "()"))
    (should= "" (parse-chunk "{}"))
    (should= "" (parse-chunk "<>"))
    (should= "" (parse-chunk "[]"))
    )

  (it "invalidates unexpected closers"
    (should= :bad-p (parse-chunk ")"))
    (should= :bad-c (parse-chunk "}"))
    (should= :bad-a (parse-chunk ">"))
    (should= :bad-p (parse-chunk "())")))

  (it "validates recursive pairs"
    (should= "" (parse-chunk "(())"))
    (should= "" (parse-chunk "((()())())"))
    )

  (it "validates consecutive chunks"
    (should= "" (parse-chunk "()()")))

  (it "validates sample valid chunks"
    (should= "" (parse-chunk "([])"))
    (should= "" (parse-chunk "{()()()}"))
    (should= "" (parse-chunk "<([{}])>"))
    (should= "" (parse-chunk "[<>({}){}[([])<>]]"))
    (should= "" (parse-chunk "(((((((((())))))))))")))

  (it "invalidates sample corrupted chunks"
    (should= :bad-b (parse-chunk "]"))
    (should= :bad-b (parse-chunk "]]]"))
    (should= :bad-a (parse-chunk "{()()()>"))
    (should= :bad-c (parse-chunk "(((()))}"))
    (should= :bad-p (parse-chunk "<([]){()}[{}])"))
    (should= :bad-a (parse-chunk "(())>>)"))
    (should= :bad-b (parse-chunk "[(]"))
    )

  (it "identifies incomplete chunks"
    (should= ")" (parse-chunk "(()"))
    (should= "}}]])})]" (parse-chunk "[({([[{{"))
    (should= "}}]])})]" (parse-chunk "[({(<(())[]>[[{[]{<()<>>"))
    (should= "" (parse-chunk "()"))
    (should= ")" (parse-chunk "("))
    (should= "))" (parse-chunk "(("))
    (should= ")]" (parse-chunk "[("))
    (should= "}}]])})]" (parse-chunk "[({(<(())[]>[[{[]{<()<>>")))
  )

(def test-data (str "[({(<(())[]>[[{[]{<()<>>\n"
                    "[(()[<>])]({[<{<<[]>>(\n"
                    "{([(<{}[<>[]}>{[]{[(<()>\n"
                    "(((({<>}<{<{<>}{[]{[]{}\n"
                    "[[<[([]))<([[{}[[()]]]\n"
                    "[{[{({}]{}}([{[{{{}}([]\n"
                    "{<[[]]>}<{[{[{[]{()[[[]\n"
                    "[<(<(<(<{}))><([]([]()\n"
                    "<{([([[(<>()){}]>(<<{{\n"
                    "<{([{{}}[<[[[<>{}]]]>[]]\n"))

(describe "solutions"
  (context "part 1"
    (it "solves the sample data"
      (should= 26397 (solve-1 test-data)))

    (it "solves the problem"
      (should= 278475 (solve-1 (slurp "input")))))

  (context "part 2"
    (it "solves the sample data"
      (should= 288957 (solve-2 test-data)))

    (it "solves the problem"
      (should= 3015539998 (solve-2 (slurp "input"))))))

