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

;; translators for the different commands

(defn translate-add
  "Translates the'add' vm command to hack assembly"
  [cmd]
  (s/join "\n" [(pop-to-d-dec-a)
                "M=D+M"
                (inc-a-update-sp)]))

(defn translate-sub
  "Translates the 'sub' vm command to hack assembly"
  [cmd]
  (s/join "\n" [(pop-to-d-dec-a)
                "M=M-D"
                (inc-a-update-sp)]))

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
                (inc-a-update-sp)]))

(defn translate-or
  "Translates the 'or' vm command to hack assembly.
  0x0000 is false and 0xffff is true."
  [cmd]
  (s/join "\n" [(pop-to-d-dec-a)
                "M=D|M"
                (inc-a-update-sp)]))

(defn translate-not
  "Translates the 'not' vm command to hack assembly.
  0x0000 is false and 0xffff is true."
  [cmd]
  (s/join "\n" [(point-a-to-stack-top)
                "M=!M"]))

(defn translate-push-constant
  "Translates the 'push constant' command to assembly"
  [{:keys [index] :as cmd}]
  (s/join "\n" [; store constant in D
                (str "@" index)
                "D=A"
                (push-from-d)
                (inc-sp)]))

(defn translate-push
  "Translates the 'push' command"
  ; TODO implement for other segments and for nil
  [{:keys [segment] :as cmd}]
  (case segment
    "constant" (translate-push-constant cmd)))

(defn find-translator
  "Finds the appropriate function to translate the given command"
  [{:keys [command] :as cmd}]
  (case command
    "add" translate-add
    "sub" translate-sub
    "neg" translate-neg
    "and" translate-and
    "or" translate-or
    "not" translate-not
    "push" translate-push
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
