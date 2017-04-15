(ns vm-translator.code
  (:require [clojure.string :as s]))

;; helpers to generate common hack asm code snippets
(defn- pop-to-d
  "Generates asm code to pop value from stack to D register"
  []
  (s/join "\n" ["@SP"
                "A=M-1"
                "D=M"]))

(defn- dec-a
  "Generates asm code to decrement A register"
  []
  "A=A-1")

(defn- pop-to-d-dec-a
  "Generates asm code that pops value from stack to D register
  then decrements A register"
  []
  (s/join "\n" [(pop-to-d)
                (dec-a)]))

(defn- push-from-d
  "Generates asm code to push value to stack from D register"
  []
  (s/join "\n" ["@SP"
                "A=M"
                "M=D"]))

(defn- inc-sp
  "Generates asm code to increment stack pointer"
  []
  (s/join "\n" ["@SP"
                "M=M+1"]))

(defn- dec-sp
  "Generates asm code to decrement stack pointer"
  []
  (s/join "\n" ["@SP"
                "M=M-1"]))

(defn- inc-a-update-sp
  "Generates asm code to update the stack pointer by first
  incrementing the A register"
  []
  (s/join "\n" ["D=A+1"
                "@SP"
                "M=D"]))

(defn- point-a-to-stack-top
  "Generates asm code to set the A register to point to
  the value at the top of the stack."
  []
  (s/join "\n" ["@SP"
                "A=M-1"]))

(defn- at-address
  "Generates asm code to set A register to specified address."
  [address]
  (str "@" address))

(defn- store-segment-val-in-d
  "Generates asm code that stores M[base + index] in D."
  [base index]
  (s/join "\n" [(at-address index)
                "D=A"
                (at-address base)
                "A=D+M"
                "D=M"]))

(defn- store-segment-addr-in-d
  "Generates asm code that stores address base + index in D."
  [base index]
  (s/join "\n" [(at-address index)
                "D=A"
                (at-address base)
                "A=D+M"
                "D=A"]))

(defn- store-d-in-r12-addr
  "Generates asm code that stores D in M[R12]"
  []
  (s/join "\n" ["@R12"
                "A=M"
                "M=D"]))

(defn- store-d-in-r12
  "Generates asm code that stores D in R12."
  []
  (s/join "\n" ["@R12"
                "M=D"]))

;; translators for the different commands

(defn translate-add
  "Translates the'add' vm command to hack assembly"
  [cmd]
  (s/join "\n" [(pop-to-d-dec-a)
                "M=D+M"
                (dec-sp)]))

(defn translate-sub
  "Translates the 'sub' vm command to hack assembly"
  [cmd]
  (s/join "\n" [(pop-to-d-dec-a)
                "M=M-D"
                (dec-sp)]))

(defn translate-neg
  "Translates the 'neg' vm command to hack assembly"
  [cmd]
  (s/join "\n" [(point-a-to-stack-top)
                "M=-M"]))

(defn translate-and
  "Transaltes the 'and' vm command to hack assembly.
  0x0000 is false and 0xffff is true."
  [cmd]
  (s/join "\n" [(pop-to-d-dec-a)
                "M=D&M"
                (dec-sp)]))

(defn translate-or
  "Translates the 'or' vm command to hack assembly.
  0x0000 is false and 0xffff is true."
  [cmd]
  (s/join "\n" [(pop-to-d-dec-a)
                "M=D|M"
                (dec-sp)]))

(defn translate-not
  "Translates the 'not' vm command to hack assembly.
  0x0000 is false and 0xffff is true."
  [cmd]
  (s/join "\n" [(point-a-to-stack-top)
                "M=!M"]))

(defn- translate-comp
  "Translate a comparison vm command to hack assembly.
  The jump statement is the difference between the different
  comparison commands.
  0x0000 is false and 0xffff is true."
  [{{ic :instruction-number} :context} jump]
  (s/join "\n" [(pop-to-d-dec-a)
                "D=M-D"
                (at-address (+ ic 10))
                (str "D;" jump)
                (at-address (+ ic 13))
                "0;JMP"
                "D=-1"
                (at-address (+ ic 14))
                "0;JMP"
                "D=0"
                (point-a-to-stack-top)
                "A=A-1"
                "M=D"
                (dec-sp)]))


(defn translate-eq
  "Translates the 'eq' vm command to hack assembly.
  0x0000 is false and 0xffff is true."
  [cmd]
  (translate-comp cmd "JEQ"))

(defn translate-gt
  "Translates the 'gt' vm command to hack assembly.
  0x0000 is false and 0xffff is true."
  [cmd]
  (translate-comp cmd "JGT"))

(defn translate-lt
  "Translate the 'lt' vm command to hack assembly.
  0x0000 is false and 0xffff is true."
  [cmd]
  (translate-comp cmd "JLT"))


(defn translate-push-constant
  "Translates the 'push constant' command to assembly"
  [{:keys [index] :as cmd}]
  (s/join "\n" [; store constant in D
                (str "@" index)
                "D=A"
                (push-from-d)
                (inc-sp)]))

(defn- translate-generic-push
  "Translates the 'push' command for local, arg, this, that segments"
  [base index]
  (s/join "\n" [(store-segment-val-in-d base index)
                (push-from-d)
                (inc-sp)]))

(defn- translate-generic-pop
  "Translates the 'pop' command for local, arg, this, that segments."
  [base index]
  (s/join "\n" [(store-segment-addr-in-d base index)
                (store-d-in-r12)
                (pop-to-d)
                (store-d-in-r12-addr)
                (dec-sp)]))

(defn translate-push
  "Translates the 'push' command"
  ; TODO implement for other segments and for nil
  [{:keys [segment index] :as cmd}]
  (case segment
    "constant" (translate-push-constant cmd)
    "local" (translate-generic-push "LCL" index)
    "arg" (translate-generic-push "ARG" index)
    "this" (translate-generic-push "THIS" index)
    "that" (translate-generic-push "THAT" index)
    "temp" (translate-generic-push "5" index)))

(defn translate-pop
  "Translates 'pop' command to hack assembly."
  [{:keys [segment index] :as cmd}]
  (case segment
    "local" (translate-generic-pop "LCL" index)
    "arg" (translate-generic-pop "ARG" index)
    "this" (translate-generic-pop "THIS" index)
    "that" (translate-generic-pop "THAT" index)
    "temp" (translate-generic-pop "5" index)))

(defn find-translator
  "Finds the appropriate function to translate the given command"
  [{:keys [command] :as cmd}]
  (case command
    "add" translate-add
    "sub" translate-sub
    "neg" translate-neg
    "and" translate-and
    "or" translate-or
    "eq" translate-eq
    "gt" translate-gt
    "lt" translate-lt
    "not" translate-not
    "push" translate-push
    "pop" translate-pop
    nil))

(defn translate
  "Translates the specific cmd to hack assembly. Returns nil if
  it's not a valid command"
  [cmd]
  (if-let [f (find-translator cmd)]
    (f cmd)
    nil))

(defn translate-with-comment
  "Translates cmd into hack assembly and adds a comment on top
  of the resulting code. Return nil if command is not valid."
  [{:keys [source] :as cmd}]
  (if-let [out (translate cmd)]
    (str "// " source "\n" out)
    nil))
