(ns vm-translator.code
  (:require [clojure.string :as s]))

;; base address of temp segment
(def TEMP-BASE 5)

;; map linking pointer segment index to corresponding base pointer
(def pointer-segment-index-map
  {0 "THIS"
   1 "THAT"})

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

(defn- push-d-inc-sp
  "Generates asm code to push value to stack from D then
  increment stack pointer."
  []
  (s/join "\n" [(push-from-d)
                (inc-sp)]))

(defn- pop-d-dec-sp
  "Generates asm code to pop value from stack into D and
  decrement stack pointer."
  []
  (s/join "\n" ["@SP"
                "M=M-1"
                "A=M"
                "D=M"]))

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

(defn- at
  "Generates asm code to set A register to specified address."
  [address]
  (str "@" address))

(defn- store-segment-val-in-d
  "Generates asm code that stores M[base + index] in D."
  [base index]
  (s/join "\n" [(at index)
                "D=A"
                (at base)
                "A=D+M"
                "D=M"]))

(defn- store-segment-addr-in-d
  "Generates asm code that stores address base + index in D."
  [base index]
  (s/join "\n" [(at index)
                "D=A"
                (at base)
                "A=D+M"
                "D=A"]))

(defn- store-d-in-r13-addr
  "Generates asm code that stores D in M[r13]"
  []
  (s/join "\n" ["@R13"
                "A=M"
                "M=D"]))

(defn- store-d-in-r13
  "Generates asm code that stores D in r13."
  []
  (s/join "\n" ["@R13"
                "M=D"]))

(defn- prefix-label
  "Prefix label if generated within a function context"
  [label {:keys [function]}]
  (if function
    (str function "$" label)
    label))

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
                (at (+ ic 10))
                (str "D;" jump)
                (at (+ ic 13))
                "0;JMP"
                "D=-1"
                (at (+ ic 14))
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
  (s/join "\n" [(at index)
                "D=A"
                (push-from-d)
                (inc-sp)]))

(defn translate-push-temp
  "Translates the 'push temp' command to assembly"
  [{:keys [index] :as cmd}]
  (s/join "\n" [(at (+ TEMP-BASE index))
                "D=M"
                (push-d-inc-sp)]))

(defn translate-pop-temp
  "Translates the 'pop temp' command to assembly"
  [{:keys [index] :as command}]
  (s/join "\n" [(pop-to-d)
                (at (+ TEMP-BASE index))
                "M=D"
                (dec-sp)]))

(defn- translate-push-pointer-base
  "Translate 'push pointer' based on the specified base pointer.
  base should be THIS or THAT"
  [base]
  (s/join "\n" [(at base)
                "D=M"
                (push-d-inc-sp)]))

(defn- translate-pop-pointer-base
  "Translate 'pop pointer' based on the specified base pointer.
  base should be THIS or THAT"
  [base]
  (s/join "\n" [(pop-d-dec-sp)
                (at base)
                "M=D"]))

(defn translate-push-pointer
  "Translate the 'push pointer' command"
  [{:keys [index]}]
  (let [base (get pointer-segment-index-map index)]
    (translate-push-pointer-base base)))


(defn translate-pop-pointer
  "Translates 'pop pointer' command to assembly"
  [{:keys [index]}]
  (let [base (get pointer-segment-index-map index)]
    (translate-pop-pointer-base base)))

(defn translate-push-static
  [{index :index {class :class} :context}]
  (s/join "\n" [(at (str class "." index))
                "D=M"
                (push-d-inc-sp)]))

(defn translate-pop-static
  [{index :index {class :class} :context}]
  (s/join "\n" [(pop-d-dec-sp)
                (at (str class "." index))
                "M=D"]))

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
                (store-d-in-r13)
                (pop-to-d)
                (store-d-in-r13-addr)
                (dec-sp)]))

(defn translate-push
  "Translates the 'push' command"
  ; TODO implement for other segments and for nil
  [{:keys [segment index] :as cmd}]
  (case segment
    "constant" (translate-push-constant cmd)
    "local" (translate-generic-push "LCL" index)
    "argument" (translate-generic-push "ARG" index)
    "this" (translate-generic-push "THIS" index)
    "that" (translate-generic-push "THAT" index)
    "temp" (translate-push-temp cmd)
    "pointer" (translate-push-pointer cmd)
    "static" (translate-push-static cmd)))

(defn translate-pop
  "Translates 'pop' command to hack assembly."
  [{:keys [segment index] :as cmd}]
  (case segment
    "local" (translate-generic-pop "LCL" index)
    "argument" (translate-generic-pop "ARG" index)
    "this" (translate-generic-pop "THIS" index)
    "that" (translate-generic-pop "THAT" index)
    "temp" (translate-pop-temp cmd)
    "pointer" (translate-pop-pointer cmd)
    "static" (translate-pop-static cmd)))

(defn translate-label
  "Translates 'label' command to hack assembly."
  [{:keys [label context] :as cmd}]
  (let [label (prefix-label label context)]
    (str "(" label ")")))

(defn translate-goto
  "Translates 'goto' vm command to assembly."
  [{:keys [label context] :as cmd}]
  (let [label (prefix-label label context)]
    (s/join "\n" [(at label)
                  "0;JMP"])))

(defn translate-if-goto
  "Translates 'if-goto' vm command to assembly."
  [{:keys [label context] :as cmd}]
  (let [label (prefix-label label context)]
    (s/join "\n" [(pop-d-dec-sp)
                  (at label)
                  "D;JNE"])))

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
    "label" translate-label
    "goto" translate-goto
    "if-goto" translate-if-goto
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
