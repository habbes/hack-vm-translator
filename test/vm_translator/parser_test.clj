(ns vm-translator.parser-test
  (:require [clojure.test :refer :all]
            [vm-translator.parser :refer :all]))

(deftest clean-line-test
  (testing "command only"
    (let [command (clean-line "push local 0")]
      (is (= "push local 0" command)))
    (let [command (clean-line "add")]
      (is (= "add"))))
  (testing "empty string"
    (let [command (clean-line "")]
      (is (= "" command))))
  (testing "whitespace only returns empty string"
    (let [command (clean-line "   ")]
      (is (= "" command)))
    (let [command (clean-line "     ")]
      (is (= "" command))))
  (testing "comments only become empty string"
    (let [command (clean-line "// this is a test")]
      (is (= "" command)))
    (let [command (clean-line "  //this is a test")]
      (is (= "" command)))
    (let [command (clean-line "//")]
      (is (= "" command))))
  (testing "ignores whitespace and comments returns command only"
    (let [command (clean-line "add // this is a test")]
      (is (= "add" command)))
    (let [command (clean-line "    pop local  2")]
      (is (= "pop local 2")))
    (let [command (clean-line "  push   constant 3 // comment")]
      (is (= "push constant 3")))))

(deftest parser-arithmetic-command-test
  (testing "returns the same command and source as in context object"
    (let [cmd (parse-arithmetic-command {:source "add" :command "add"} [])]
      (is (= cmd {:source "add" :command "add"})))
    (let [cmd (parse-arithmetic-command {:source "neg" :command "neg"} [])]
      (is (= cmd {:source "neg" :command "neg"})))))

(deftest parse-push-pop-command-test
  (testing "adds segment and index to context"
    (let [cmd
          (parse-push-pop-command
            {:source "push local 1" :command "push"} ["local" "1"])]
      (is (= cmd {:source "push local 1" :command "push"
                  :segment "local" :index 1})))
    (let [cmd
          (parse-push-pop-command
            {:source "pop that 5" :command "pop"} ["that" "5"])]
      (is (= cmd {:source "pop that 5" :command "pop"
                  :segment "that" :index 5})))))

(deftest parse-if-match-test
  (let [re #"(\w+) (\w+)"
        f (fn [cmd [arg]] (assoc cmd :arg arg))]
    (testing "parses with given fn if re matches"
      (let [source "cmd myarg" cmd (parse-if-match source re f)]
        (is (= {:source "cmd myarg"
                :command "cmd"
                :arg "myarg"}
               cmd))))
    (testing "returns nil if re doesn't match"
      (let [source "cmd" cmd (parse-if-match source re f)]
        (is (= nil cmd))))))

(deftest match-and-parse-test
  (let [re-f-pairs [[#"(\w+) (\w+)" (fn [cmd [arg]] (assoc cmd :arg arg))]
                    [#"(\w+)" (fn [cmd _] cmd)]]]
    (testing "parses with matching f if source has match"
      (let [source "cmd 1" cmd (match-and-parse source re-f-pairs)]
        (is (= {:source "cmd 1"
                :command "cmd"
                :arg "1"}
               cmd)))
      (let [source "cmd" cmd (match-and-parse source re-f-pairs)]
        (is (= {:source "cmd"
                :command "cmd"}
               cmd))))
    (testing "throws exception if there is no match"
      (let [source "this has no match"]
        (is (thrown-with-msg? Exception
                              #"Cannot parse"
                              (match-and-parse source re-f-pairs)))))))

(deftest parse-command-with-def-matchers
  (let [segments ["argument" "this" "that" "local"
                  "constant" "temp" "static" "pointer"]]
    (testing "push commands"
      (doseq [segment segments]
        (let [source (str "push " segment " 2")
              cmd (parse-command source)]
          (is (= {:source source
                  :command "push"
                  :segment segment
                  :index 2}
                 cmd)))))
    (testing "pop commands"
      (doseq [segment segments]
        (let [source (str "pop " segment " 2")
              cmd (parse-command source)]
          (is (= {:source source
                  :command "pop"
                  :segment segment
                  :index 2}
                 cmd))))))
  (testing "arithmetic/logic commands"
    (doseq [source ["add" "sub" "neg" "eq" "gt" "lt" "and" "or" "not"]]
      (let [cmd (parse-command source)]
        (is (= {:source source
                :command source}
               cmd)))))
  (testing "branching commands"
    (let [branches [["goto" "BASECASE"] ["if-goto" "LOOP_END"] ["goto" "Foo.bar$LOOP"]]]
      (doseq [[command label] branches]
        (let [source (str command " " label)
              cmd (parse-command source)]
          (is (= {:source source
                  :command command
                  :label label}
                 cmd))))))
  (testing "function command"
    (let [source "function Math.multiply 2"
          cmd (parse-command source)]
      (is (= {:source source
              :command "function"
              :function "Math.multiply"
              :vars 2}
             cmd)))
    (let [source "function factorial 1"
          cmd (parse-command source)]
      (is (= {:source source
              :command "function"
              :function "factorial"
              :vars 1}
             cmd))))

  (testing "call command"
    (let [source "call Math.multiply 2"
          cmd (parse-command source)]
      (is (= cmd {:source source
                  :command "call"
                  :function "Math.multiply"
                  :args 2})))
    (let [source "call discriminant 3"
          cmd (parse-command source)]
      (is (= cmd {:source source
                  :command "call"
                  :function "discriminant"
                  :args 3}))))

  (testing "return command"
    (let [source "return"
          cmd (parse-command source)]
      (is (= cmd {:source source
                  :command "return"}))))

  (testing "ignores comments and extra whitespace"
    (let [source "push local 10 //test" cmd (parse-command source)]
      (is (= {:source "push local 10"
              :command "push"
              :segment "local"
              :index 10}
             cmd)))
    (let [source "\tadd  " cmd (parse-command source)]
      (is (= {:source "add"
              :command "add"}
             cmd)))
    (let [source " pop  pointer  0 // comment" cmd (parse-command source)]
      (is (= {:source "pop pointer 0"
              :command "pop"
              :segment "pointer"
              :index 0}
              cmd))))
  (testing "returns nil if no command or no match"
    (let [sources ["" "   " "// this is a comment" "\t // this is a comment"]]
      (doseq [source sources]
        (is (= {:source nil
                :command nil}
               (parse-command source))))))

  (testing "throws exception if no match found for source"
    (let [sources ["push local" "this is not a command// push argument 1"]]
      (doseq [source sources]
        (is (thrown-with-msg? Exception
                              #"Cannot parse"
                              (parse-command source)))))))

