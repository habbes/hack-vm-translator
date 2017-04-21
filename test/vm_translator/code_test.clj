(ns vm-translator.code-test
  (:require [clojure.test :refer :all]
            [vm-translator.code :refer :all]
            [clojure.string :as s]))

(deftest translate-test
  (testing "add command"
    (let [cmd {:source "add" :command "add"}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "D=M"
                                "A=A-1"
                                "M=D+M"
                                "@SP"
                                "M=M-1"])))))
  (testing "sub command"
    (let [cmd {:source "sub" :command "sub"}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "D=M"
                                "A=A-1"
                                "M=M-D"
                                "@SP"
                                "M=M-1"])))))
  (testing "neg command"
    (let [cmd {:source "neg" :command "neg"}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "M=-M"])))))

  (testing "and command"
    (let [cmd {:source "and" :command "and"}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "D=M"
                                "A=A-1"
                                "M=D&M"
                                "@SP"
                                "M=M-1"])))))

  (testing "or command"
    (let [cmd {:source "or" :command "or"}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "D=M"
                                "A=A-1"
                                "M=D|M"
                                "@SP"
                                "M=M-1"])))))

  (testing "not command"
    (let [cmd {:source "not" :command "not"}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "M=!M"])))))

  (testing "eq command"
    (let [cmd {:source "eq" :command "eq"
               :context {:instruction-number 5}}
          [code ctx] (translate cmd)]
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
          [code ctx] (translate cmd)]
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
          [code ctx] (translate cmd)]
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
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@17"
                                "D=A"
                                "@SP"
                                "A=M"
                                "M=D"
                                "@SP"
                                "M=M+1"])))))

  (testing "push local/argument/this/that commands"
    (let [segment-var-map {"local" "LCL"
                           "argument" "ARG"
                           "this" "THIS"
                           "that" "THAT"}]
      (doseq [[segment base] segment-var-map]
        (let [cmd {:source (str "push " segment " 3")
                   :command "push"
                   :segment segment
                   :index 3}
              [code ctx] (translate cmd)]
          (is (= code (s/join "\n" ["@3"
                                    "D=A"
                                    (str "@" base)
                                    "A=D+M"
                                    "D=M"
                                    "@SP"
                                    "A=M"
                                    "M=D"
                                    "@SP"
                                    "M=M+1"])))))))

  (testing "pop local/argument/this/that commands"
    (let [segment-var-map {"local" "LCL"
                           "argument" "ARG"
                           "this" "THIS"
                           "that" "THAT"}]
      (doseq [[segment base] segment-var-map]
        (let [cmd {:source (str "pop " segment " 7")
                   :command "pop"
                   :segment segment
                   :index 7}
              [code ctx] (translate cmd)]
          (is (= code (s/join "\n" ["@7"
                                    "D=A"
                                    (str "@" base)
                                    "A=D+M"
                                    "D=A"
                                    "@R13"
                                    "M=D"
                                    "@SP"
                                    "A=M-1"
                                    "D=M"
                                    "@R13"
                                    "A=M"
                                    "M=D"
                                    "@SP"
                                    "M=M-1"])))))))

  (testing "push temp command"
    (let [cmd {:source "push temp 4"
               :command "push"
               :segment "temp"
               :index 4}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@9"
                                "D=M"
                                "@SP"
                                "A=M"
                                "M=D"
                                "@SP"
                                "M=M+1"])))))

  (testing "pop temp command"
    (let [cmd {:source "pop temp 3"
               :command "pop"
               :segment "temp"
               :index 3}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "A=M-1"
                                "D=M"
                                "@8"
                                "M=D"
                                "@SP"
                                "M=M-1"])))))

  (testing "push pointer command"
    (let [cmd {:source "push pointer 0"
               :command "push"
               :segment "pointer"
               :index 0}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@THIS"
                                "D=M"
                                "@SP"
                                "A=M"
                                "M=D"
                                "@SP"
                                "M=M+1"]))))
    (let [cmd {:source "push pointer 1"
               :command "push"
               :segment "pointer"
               :index 1}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@THAT"
                                "D=M"
                                "@SP"
                                "A=M"
                                "M=D"
                                "@SP"
                                "M=M+1"])))))

  (testing "pop pointer command"
    (let [cmd {:source "pop pointer 0"
               :command "pop"
               :segment "pointer"
               :index 0}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "M=M-1"
                                "A=M"
                                "D=M"
                                "@THIS"
                                "M=D"]))))

    (let [cmd {:source "pop pointer 1"
               :command "pop"
               :segment "pointer"
               :index 1}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "M=M-1"
                                "A=M"
                                "D=M"
                                "@THAT"
                                "M=D"])))))

  (testing "push static command"
    (let [cmd {:source "push static 3"
               :command "push"
               :segment "static"
               :index 3
               :context {:class "Foo"}}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@Foo.3"
                                "D=M"
                                "@SP"
                                "A=M"
                                "M=D"
                                "@SP"
                                "M=M+1"])))))

  (testing "pop static command"
    (let [cmd {:source "pop static 5"
               :command "pop"
               :segment "static"
               :index 5
               :context {:class "Bar"}}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "M=M-1"
                                "A=M"
                                "D=M"
                                "@Bar.5"
                                "M=D"])))))

  (testing "label command"
    (let [cmd {:source "label END"
               :command "label"
               :label "END"}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["(END)"]))))
    (testing "prefixes current function name within function context"
      (let [cmd {:source "label LOOP"
                 :command "label"
                 :label "LOOP"
                 :context {:class "MyClass"
                           :function "MyClass.func"} }
            [code ctx] (translate cmd)]
        (is (= code "(MyClass.func$LOOP)")))))

  (testing "goto command"
    (let [cmd {:source "goto LOOP"
               :command "goto"
               :label "LOOP"}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@LOOP"
                                "0;JMP"]))))
    (testing "prefixes function name within function context"
      (let [cmd {:source "goto BASE"
                 :command "goto"
                 :label "BASE"
                 :context {:class "Foo"
                           :function "Foo.funct"}}
            [code ctx] (translate cmd)]
        (is (= code (s/join "\n" ["@Foo.funct$BASE"
                                  "0;JMP"]))))))

  (testing "if-goto command"
    (let [cmd {:source "if-goto LOOP_END"
               :command "if-goto"
               :label "LOOP_END"}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["@SP"
                                "M=M-1"
                                "A=M"
                                "D=M"
                                "@LOOP_END"
                                "D;JNE"]))))
    (testing "prefixes function name within function context"
      (let [cmd {:source "if-goto END"
                 :command "if-goto"
                 :label "END"
                 :context {:class "Foo"
                           :function "Foo.funct"}}
            [code ctx] (translate cmd)]
        (is (= code (s/join "\n" ["@SP"
                                  "M=M-1"
                                  "A=M"
                                  "D=M"
                                  "@Foo.funct$END"
                                  "D;JNE"]))))))

  (testing "function command"
    (let [cmd {:source "function MyClass.func 3"
               :command "function"
               :function "MyClass.func"
               :vars 3}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" ["(MyClass.func)"
                                "@SP"
                                "A=M"
                                "M=0"
                                "A=A+1"
                                "M=0"
                                "A=A+1"
                                "M=0"
                                "D=A+1"
                                "@SP"
                                "M=D"])))
      (testing "sets context function"
        (is (= (:function ctx) "MyClass.func"))))

    (testing "with 0 vars"
      (let [cmd {:source "function Sys.init 0"
                 :command "function"
                 :function "Sys.init"
                 :vars 0}
            [code ctx] (translate cmd)]
        (is (= code "(Sys.init)")))))

  (testing "return command"
    (let [cmd {:source "return"
               :command "return"
               :context {:function "Math.pow"
                         :instruction-number 10}}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" [; store the top address of the current frame
                                "@LCL"
                                "D=M"
                                "@R14"
                                "M=D"
                                ; store return address in R15
                                "@5"
                                "A=D-A"
                                "D=M"
                                "@R15"
                                "M=D"
                                ; pop from stack and store in ARG
                                "@SP"
                                "A=M-1"
                                "D=M"
                                "@ARG"
                                "A=M"
                                "M=D"
                                ; restore caller's SP
                                "@ARG"
                                "D=M+1"
                                "@SP"
                                "M=D"
                                ; restore THAT
                                "@R14"
                                "AM=M-1"
                                "D=M"
                                "@THAT"
                                "M=D"
                                ; restore THIS
                                "@R14"
                                "AM=M-1"
                                "D=M"
                                "@THIS"
                                "M=D"
                                ; restore ARG
                                "@R14"
                                "AM=M-1"
                                "D=M"
                                "@ARG"
                                "M=D"
                                ; restore LCL
                                "@R14"
                                "AM=M-1"
                                "D=M"
                                "@LCL"
                                "M=D"
                                ; goto return address
                                "@R15"
                                "A=M"
                                "0;JMP"])))
      (testing "unsets function from context"
        (is (= (contains? ctx :function) false)))))

  (testing "call command"
    (let [cmd {:source "call SomeClass.test 3"
               :command "call"
               :function "SomeClass.test"
               :args 3
               :context {:instruction-number 4}}
          [code ctx] (translate cmd)]
      (is (= code (s/join "\n" [;; save caller's frame onto stack
                                ;push return address
                                "@47" ;4 + 43 instructions in this block
                                "D=A"
                                "@SP"
                                "A=M"
                                "M=D"
                                "@SP"
                                "M=M+1"
                                ; push LCL
                                "@LCL"
                                "D=M"
                                "@SP"
                                "A=M"
                                "M=D"
                                "@SP"
                                "M=M+1"
                                ; push ARG
                                "@ARG"
                                "D=M"
                                "@SP"
                                "A=M"
                                "M=D"
                                "@SP"
                                "M=M+1"
                                ; push THIS
                                "@THIS"
                                "D=M"
                                "@SP"
                                "A=M"
                                "M=D"
                                "@SP"
                                "M=M+1"
                                ; push THAT
                                "@THAT"
                                "D=M"
                                "@SP"
                                "A=M"
                                "M=D"
                                "@SP"
                                "M=M+1"
                                ;; reposition callee's ARG
                                "@8" ; 3 (args) + 5 (frame size)
                                "D=A"
                                "@SP"
                                "D=M-D"
                                "@ARG"
                                "M=D"
                                ;; goto function
                                "@SomeClass.test"
                                "0;JMP"])))))

  (testing "empty command"
    (let [cmd {:source nil
               :command nil}
          [code ctx] (translate cmd)]
      (is (= code nil))))

  (testing "throws exception on invalid command"
    (doseq [cmd [{:command "invalid"}]]
      (is (thrown-with-msg? Exception
                            #"Cannot translate invalid command"
                            (translate cmd))))))

(deftest translate-with-comment-test
  (testing "Adds source vm code as comment before output code"
    (let [cmd {:source "push constant 17"
               :command "push"
               :segment "constant"
               :index 17}
          [code ctx] (translate-with-comment cmd)]
      (is (= code (s/join "\n" ["// push constant 17"
                                "@17"
                                "D=A"
                                "@SP"
                                "A=M"
                                "M=D"
                                "@SP"
                                "M=M+1"])))))
  (testing "Does not add comment if command is empty"
    (let [cmd {:source nil
               :command nil}
          [code ctx] (translate-with-comment cmd)]
      (is (= code nil)))))
