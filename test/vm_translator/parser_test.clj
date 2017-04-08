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
                  :segment "local" :index "1"})))
    (let [cmd
          (parse-push-pop-command
            {:source "pop that 5" :command "pop"} ["that" "5"])]
      (is (= cmd {:source "pop that 5" :command "pop"
                  :segment "that" :index "5"})))))

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
                :arg "1"})))
      (let [source "cmd" cmd (match-and-parse source re-f-pairs)]
        (is (= {:source "cmd"
                :command "cmd"}))))
    (testing "returns in nil if there is no match"
      (let [source "this has no match" cmd (match-and-parse source re-f-pairs)]
        (is (= nil cmd))))))

