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

(deftest translate-line-test
  (testing "translates source vm line to hack assembly with comment"
    (let [line "push constant 3"
          ctx {:instruction-number 0 :line-number 1}
          [out ctx] (translate-line line ctx)]
      (is (= out (s/join "\n" ["// push constant 3"
                               "@3"
                               "D=A"
                               "@SP"
                               "A=M"
                               "M=D"
                               "@SP"
                               "M=M+1"])))
      (testing "increments line number"
        (is (= (:line-number ctx) 2)))))

  (testing "Throw exception when source is invalid"
    (let [line "not valid command"
          ctx {:instruction-number 0 :line-number 1}]
      (is (thrown-with-msg? Exception
                                #"Cannot parse"
                                (translate-line line ctx))))))

(defn make-sample-output-handler
  "Creates an output handler fn that appends the output
  to the specified out-atom"
  [out-atom]
  (fn [out]
    (swap! out-atom (fn [old] (str old out "\n")))))

(deftest translate-lines-test
  (testing "Translate seq of source lines into asm output string"
    (let [lines (s/split-lines sample-source)
          output (atom "")
          handler (make-sample-output-handler output)
          ctx {:line-number 1 :instruction-number 0}
          final-ctx (translate-lines lines handler ctx)]
      (is (= @output sample-output))
      (is (= (:line-number final-ctx 4)))
      (is (= (:instruction-number final-ctx 21)))))

  (testing "Translate lines including comments"
    (let [lines (s/split-lines sample-source-with-comments)
          output (atom "")
          handler (make-sample-output-handler output)
          ctx {:line-number 0 :instruction-number -1}
          final-ctx (translate-lines lines handler ctx)]
      (is (= @output sample-output))
      (is (= (:line-number final-ctx) 8))
      (is (= (:instruction-number final-ctx) 20))))

  (testing "Translate lines including comparison commands"
    (let [lines (s/split-lines sample-source-eq)
          output (atom "")
          handler (make-sample-output-handler output)
          ctx {:line-number 0 :instruction-number -1}
          final-ctx (translate-lines lines handler ctx)]
      (is (= @output sample-output-eq)))))

(deftest make-writer-output-handler-test
  (testing "Creates a handler that writers output to an io writer"
    (let [wrtr (java.io.StringWriter.)
          handler (make-writer-output-handler wrtr)]
      (handler "@SP\nA=M\nD=M")
      (handler "D=D+1")
      (is (= (.toString wrtr)
             "@SP\nA=M\nD=M\nD=D+1\n")))))

(deftest translate-source-test
  (testing "Translates vm source from reader and writer asm output to writer"
    (let [rdr (clojure.java.io/reader (java.io.StringReader. sample-source))
          output (atom "")
          handler (make-sample-output-handler output)
          ctx {:line-number 0
               :instruction-number -1
               :class "SampleClass"}
          out-ctx (translate-source rdr handler ctx)]
      (is (= @output sample-output))))

  (testing "Translates vm source code containing comparison commands"
    (let [rdr (clojure.java.io/reader (java.io.StringReader. sample-source-eq))
          wrtr (java.io.StringWriter.)
          output (atom "")
          handler (make-sample-output-handler output)
          ctx {:line-number 0
               :instruction-number -1
               :class "SampleClass"}
          out-ctx (translate-source rdr handler ctx)]
      (is (= @output sample-output-eq)))))

(deftest find-translator-test
  (testing "Returns translate-file if path is a file"
    (let [f (find-translator "test/test_files/SimpleAdd.vm")]
      (is (= f translate-file))))
  (testing "Returns translate-dir if path is a directory"
    (let [f (find-translator "test/test_files/NestedCall")]
      (is (= f translate-dir)))
    (let [f (find-translator "test/test_files/FibonacciElement")]
      (is (= f translate-dir)))))
