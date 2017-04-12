(ns vm-translator.code-test
  (:require [clojure.test :refer :all]
            [vm-translator.code :refer :all]
            [clojure.string :as s]))

(deftest translate-add-test
  (let [cmd {:source "add" :command "add"}
        code (translate-add cmd)]
    (is (= code (s/join "\n" ["@SP"
                              "A=M-1"
                              "D=M"
                              "A=A-1"
                              "M=M+D"
                              "D=A"
                              "@SP"
                              "M=D"])))))

(deftest translate-push-contant-test
  (let [cmd {:source "push constant 17"
             :command "push"
             :segment "constant"
             :index "17"}
        code (translate-push-constant cmd)]
    (is (= code (s/join "\n" ["@17"
                              "D=A"
                              "@SP"
                              "A=M"
                              "M=D"
                              "D=A+1"
                              "@SP"
                              "M=D"])))))

(deftest translate-test
  (testing "add command"
    (let [cmd {:source "add" :command "add"}
          code (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "D=M"
                                "A=A-1"
                                "M=M+D"
                                "D=A"
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
                                "M=D"]))))))
