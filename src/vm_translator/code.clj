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
                "D=A"
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
    "push" translate-push))

(defn translate
  "Translates the specific cmd to hack assembly. Returns nil if
  it's not a valid command"
  [cmd]
  (if-let [f (find-translator cmd)]
    (f cmd)
    nil))
