(ns vm-translator.code
  (:require [clojure.string :as s]))

(defn translate-add
  "Translates the'add' vm command to hack assembly"
  [cmd]
  (s/join "\n" [; pop value from stack to D
                "@SP"
                "A=M-1"
                "D=M"
                ; pop the next value in the stack
                ; add D and store the sum in the stack
                "A=A-1"
                "M=M+D"
                ; update the stack pointer
                "D=M"
                "@SP"
                "M=D"]))

(defn translate-push-constant
  "Translates the 'push constant' command to assembly"
  [{:keys [index] :as cmd}]
  (s/join "\n" [; store constant in D
                (str "@" index)
                "D=A"
                ; push value to stack
                "@SP"
                "A=M"
                "M=D"
                ; update stack pointer
                "D=A+1"
                "@SP"
                "M=D"]))
