(ns vm-translator.code
  (:require [clojure.string :as s]))

(defn translate-add
  "Translates add vm command to hack assembly"
  [cmd]
  (s/join "\n" [;pop value from stack to D
                "@SP"
                "A=M-1"
                "D=M"
                ; pop the next value in the stack to M
                "A=A-1"
                ; store the sum at the stack location
                "M=M+D"
                ; update the stack pointer
                "D=M"
                "@SP"
                "M=D"]))
