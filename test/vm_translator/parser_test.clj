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

