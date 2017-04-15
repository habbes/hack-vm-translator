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
                                "@SP"
                                "M=M-1"])))))
  (testing "sub command"
    (let [cmd {:source "sub" :command "sub"}
          code (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "D=M"
                                "A=A-1"
                                "M=M-D"
                                "@SP"
                                "M=M-1"])))))
  (testing "neg command"
    (let [cmd {:source "neg" :command "neg"}
          code (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "M=-M"])))))

  (testing "and command"
    (let [cmd {:source "and" :command "and"}
          code (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "D=M"
                                "A=A-1"
                                "M=D&M"
                                "@SP"
                                "M=M-1"])))))

  (testing "or command"
    (let [cmd {:source "or" :command "or"}
          code (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "D=M"
                                "A=A-1"
                                "M=D|M"
                                "@SP"
                                "M=M-1"])))))

  (testing "not command"
    (let [cmd {:source "not" :command "not"}
          code (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "M=!M"])))))

  (testing "eq command"
    (let [cmd {:source "eq" :command "eq"
               :context {:instruction-number 5}}
          code (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "D=M"
                                "A=A-1"
                                "D=M-D"
                                "@15"
                                "D;JEQ"
                                "@18"
                                "0;JMP"
                                "D=-1"
                                "@19"
                                "0;JMP"
                                "D=0"
                                "@SP"
                                "A=M-1"
                                "A=A-1"
                                "M=D"
                                "@SP"
                                "M=M-1"])))))

  (testing "gt command"
    (let [cmd {:source "gt" :command "gt"
               :context {:instruction-number 4}}
          code (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "D=M"
                                "A=A-1"
                                "D=M-D"
                                "@14"
                                "D;JGT"
                                "@17"
                                "0;JMP"
                                "D=-1"
                                "@18"
                                "0;JMP"
                                "D=0"
                                "@SP"
                                "A=M-1"
                                "A=A-1"
                                "M=D"
                                "@SP"
                                "M=M-1"])))))

  (testing "lt command"
    (let [cmd {:source "lt" :command "lt"
               :context {:instruction-number 6}}
          code (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "D=M"
                                "A=A-1"
                                "D=M-D"
                                "@16"
                                "D;JLT"
                                "@19"
                                "0;JMP"
                                "D=-1"
                                "@20"
                                "0;JMP"
                                "D=0"
                                "@SP"
                                "A=M-1"
                                "A=A-1"
                                "M=D"
                                "@SP"
                                "M=M-1"])))))

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
                                "@SP"
                                "M=M+1"])))))
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
                                "@SP"
                                "M=M+1"]))))))
