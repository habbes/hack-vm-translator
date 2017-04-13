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

(defn translate-add
  "Translates the'add' vm command to hack assembly"
  [cmd]
  (s/join "\n" [(pop-to-d)
                ; pop the next value in the stack
                ; add D and store the sum in the stack
                "A=A-1"
                "M=D+M"
                (inc-a-update-sp)]))

(defn translate-sub
  "Translates the 'sub' vm command to hack assembly"
  [cmd]
  (s/join "\n" [(pop-to-d)
                "A=A-1"
                "M=M-D"
                (inc-a-update-sp)]))
(defn translate-neg
  "Translates the 'neg' vm command to hack assembly"
  [cmd]
  (s/join "\n" ["@SP"
                "A=M-1"
                "M=-M"]))

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
