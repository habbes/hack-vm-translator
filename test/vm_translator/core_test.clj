(ns vm-translator.core-test
  (:require [clojure.test :refer :all]
            [vm-translator.core :refer :all]
            [clojure.string :as s]))

(def sample-source
"push constant 5
push constant 3
add")

(def sample-source-with-comments
"// This is a program that adds two constants
// The constants are pushed to the stack
// Then the add command is called

push constant 5
push constant 3
add
// result is 3 + 5 = 8")

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
D=A+1
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
                               "M=D"
                               ""])))))

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
  (testing "Translate seq of source lines into asm output string"
    (let [lines (s/split-lines sample-source)
          output (atom "")
          handler (create-sample-output-handler output)]
      (translate-lines lines handler)
      (is (= @output sample-output))))

  (testing "Translate lines including comments"
    (let [lines (s/split-lines sample-source-with-comments)
          output (atom "")
          handler (create-sample-output-handler output)]
      (translate-lines lines handler)
      (is (= @output sample-output)))))

(deftest create-writer-output-handler-test
  (testing "Creates a handler that writers output to an io writer"
    (let [wrtr (java.io.StringWriter.)
          handler (create-writer-output-handler wrtr)]
      (handler "@SP\nA=M\nD=M\n")
      (handler "D=D+1\n")
      (is (= (.toString wrtr)
             "@SP\nA=M\nD=M\nD=D+1\n"))))
  (testing "Skips nil"
    (let [wrtr (java.io.StringWriter.)
          handler (create-writer-output-handler wrtr)]
      (handler "@SP\nA=M\nD=M\n")
      (handler nil)
      (handler "D=D+1\n")
      (is (= (.toString wrtr)
             "@SP\nA=M\nD=M\nD=D+1\n")))))

(deftest translate-source-test
  (testing "Translates vm source from reader and writer asm output to writer"
    (let [rdr (clojure.java.io/reader (java.io.StringReader. sample-source))
          wrtr (java.io.StringWriter.)]
      (translate-source rdr wrtr)
      (is (= (.toString wrtr) sample-output)))))
