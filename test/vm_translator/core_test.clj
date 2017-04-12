(ns vm-translator.core-test
  (:require [clojure.test :refer :all]
            [vm-translator.core :refer :all]
            [clojure.string :as s]))

(def sample-source
"push constant 5
push constant 3
add")

(def sample-output
"// push constant 5
@5
D=A
@SP
A=M
M=D
D=A+1
@SP
M=D
// push constant 3
@3
D=A
@SP
A=M
M=D
D=A+1
@SP
M=D
// add
@SP
A=M-1
D=M
A=A-1
M=M+D
D=A
@SP
M=D
")

(deftest get-output-path-test
  (testing "Replace filename extension with .asm"
    (let [input "Sample.vm" out (get-output-path input)]
      (is (= out "Sample.asm"))))
  (testing "Should support absolute paths"
    (let [input "/this/is/a/Program.vm"
          out (get-output-path input)]
      (is (= out "/this/is/a/Program.asm"))))
  (testing "Should support relative paths"
    (let [input "path/to/File.vm"
          out (get-output-path input)]
      (is (= out "path/to/File.asm")))
    (let [input "../path/to/file.vm"
          out (get-output-path input)]
      (is (= out "../path/to/file.asm")))
    (let [input "./path/to/file.vm"
          out (get-output-path input)]
      (is (= out "./path/to/file.asm")))))

(deftest translate-line-test
  (testing "translates source vm line to hack assembly with comment"
    (let [line "push constant 3" out (translate-line line)]
      (is (= out (s/join "\n" ["// push constant 3"
                                "@3"
                                "D=A"
                                "@SP"
                                "A=M"
                                "M=D"
                                "D=A+1"
                                "@SP"
                                "M=D"])))))

  (testing "Returns nil on when source is invalid"
    (let [line "not valid command" out (translate-line line)]
      (is (= out nil)))))

(defn create-sample-output-handler
  "Creates an output handler fn that appends the output
  to the specified out-atom"
  [out-atom]
  (fn [out]
    (swap! out-atom (fn [old] (str old out)))))

(deftest translate-lines-test
  (testing ""
    (let [lines (s/split sample-source #"\n")
          output (atom "")
          handler (create-sample-output-handler output)]
      (translate-lines lines handler)
      (is (= @output sample-output)))))
