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
                              "D=M"
                              "@SP"
                              "M=D"])))))
