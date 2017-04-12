(ns vm-translator.code-test
  (:require [clojure.test :refer :all]
            [vm-translator.code :refer :all]
            [clojure.string :as s]))

(deftest translate-test
  (testing "add command"
    (let [cmd {:source "add" :command "add"}
          code (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "D=M"
                                "A=A-1"
                                "M=D+M"
                                "D=A+1"
                                "@SP"
                                "M=D"])))))
  (testing "sub command"
    (let [cmd {:source "sub" :command "sub"}
          code (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "D=M"
                                "A=A-1"
                                "M=M-D"
                                "D=A+1"
                                "@SP"
                                "M=D"])))))
  (testing "neg command"
    (let [cmd {:source "neg" :command "neg"}
          code (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "M=-M"
                                "D=A+1"
                                "@SP"
                                "M=D"])))))

  (testing "push constant command"
    (let [cmd {:source "push constant 17"
               :command "push"
               :segment "constant"
               :index 17}
          code (translate cmd)]
      (is (= code (s/join "\n" ["@17"
                                "D=A"
                                "@SP"
                                "A=M"
                                "M=D"
                                "D=A+1"
                                "@SP"
                                "M=D"])))))
  (testing "returns nil on invalid command"
    (let [cmd {} code (translate cmd)]
      (is (= code nil)))
    (let [cmd nil code (translate cmd)]
      (is (= code nil)))))

(deftest translate-with-comment-test
  (testing "Adds source vm code as comment before output code"
    (let [cmd {:source "push constant 17"
               :command "push"
               :segment "constant"
               :index 17}
          code (translate-with-comment cmd)]
      (is (= code (s/join "\n" ["// push constant 17"
                                "@17"
                                "D=A"
                                "@SP"
                                "A=M"
                                "M=D"
                                "D=A+1"
                                "@SP"
                                "M=D"]))))))
