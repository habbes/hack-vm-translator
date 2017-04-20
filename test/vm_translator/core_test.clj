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
@SP
M=M+1
// push constant 3
@3
D=A
@SP
A=M
M=D
@SP
M=M+1
// add
@SP
A=M-1
D=M
A=A-1
M=D+M
@SP
M=M-1
")

(def sample-source-eq
"push constant 10
push constant 10
eq")

(def sample-output-eq
"// push constant 10
@10
D=A
@SP
A=M
M=D
@SP
M=M+1
// push constant 10
@10
D=A
@SP
A=M
M=D
@SP
M=M+1
// eq
@SP
A=M-1
D=M
A=A-1
D=M-D
@23
D;JEQ
@26
0;JMP
D=-1
@27
0;JMP
D=0
@SP
A=M-1
A=A-1
M=D
@SP
M=M-1
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

(deftest get-class-name-test
  (testing "Get filename without extension"
    (let [input "Sample.vm" out (get-class-name input)]
      (is (= out "Sample"))))
  (testing "Should support absolute paths"
    (let [input "/this/is/a/Program.vm"
          out (get-class-name input)]
      (is (= out "Program"))))
  (testing "Should support relative paths"
    (let [input "path/to/File.vm"
          out (get-class-name input)]
      (is (= out "File")))
    (let [input "../path/to/file.vm"
          out (get-class-name input)]
      (is (= out "file")))
    (let [input "./path/to/file.vm"
          out (get-class-name input)]
      (is (= out "file")))))

(deftest translate-line-test
  (testing "translates source vm line to hack assembly with comment"
    (let [line "push constant 3"
          ctx {:instruction-number 0 :line-number 1}
          out (translate-line line ctx)]
      (is (= out (s/join "\n" ["// push constant 3"
                               "@3"
                               "D=A"
                               "@SP"
                               "A=M"
                               "M=D"
                               "@SP"
                               "M=M+1"])))))

  (testing "Throw exception when source is invalid"
    (let [line "not valid command"
          ctx {:instruction-number 0 :line-number 1}]
      (is (thrown-with-msg? Exception
                                #"Cannot parse"
                                (translate-line line ctx))))))

(defn create-sample-output-handler
  "Creates an output handler fn that appends the output
  to the specified out-atom"
  [out-atom]
  (fn [out]
    (swap! out-atom (fn [old] (str old out "\n")))))

(deftest translate-lines-test
  (testing "Translate seq of source lines into asm output string"
    (let [lines (s/split-lines sample-source)
          output (atom "")
          handler (create-sample-output-handler output)
          ctx {:line-number 1 :instruction-number 0}]
      (translate-lines lines handler ctx)
      (is (= @output sample-output))))

  (testing "Translate lines including comments"
    (let [lines (s/split-lines sample-source-with-comments)
          output (atom "")
          handler (create-sample-output-handler output)
          ctx {:line-number 1 :instruction-number 0}]
      (translate-lines lines handler ctx)
      (is (= @output sample-output))))

  (testing "Translate lines including comparison commands"
    (let [lines (s/split-lines sample-source-eq)
          output (atom "")
          handler (create-sample-output-handler output)
          ctx {:line-number 0 :instruction-number -1}]
      (translate-lines lines handler ctx)
      (is (= @output sample-output-eq)))))

(deftest create-writer-output-handler-test
  (testing "Creates a handler that writers output to an io writer"
    (let [wrtr (java.io.StringWriter.)
          handler (create-writer-output-handler wrtr)]
      (handler "@SP\nA=M\nD=M")
      (handler "D=D+1")
      (is (= (.toString wrtr)
             "@SP\nA=M\nD=M\nD=D+1\n")))))

(deftest translate-source-test
  (testing "Translates vm source from reader and writer asm output to writer"
    (let [rdr (clojure.java.io/reader (java.io.StringReader. sample-source))
          wrtr (java.io.StringWriter.)]
      (translate-source rdr wrtr "SampleClass")
      (is (= (.toString wrtr) sample-output))))

  (testing "Translates vm source code containing comparison commands"
    (let [rdr (clojure.java.io/reader (java.io.StringReader. sample-source-eq))
          wrtr (java.io.StringWriter.)]
      (translate-source rdr wrtr "SampleClass")
      (is (= (.toString wrtr) sample-output-eq)))))
