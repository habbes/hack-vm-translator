(ns vm-translator.parser
  (:require [clojure.string :as s]))


; regex used to parse empty lines
(def empty-re #"^$")
; regex used to parse arithmetic and logic commands
(def arithmetic-re #"(add|sub|neg|eq|gt|lt|and|or|not)")
; regex used to parse push and pop commands
(def push-pop-re #"(push|pop) (argument|local|this|that|constant|static|temp|pointer) (\d+)")
; regex used to parse branching commands
(def branching-re #"(goto|if\-goto|label) ([\w\.\-\$]+)")
; regex used to parse the function command
(def function-re #"(function) ([\w\.]+) (\d+)")
; regex used to parse the call command
(def call-re #"(call) ([\w\.]+) (\d+)")
; regex used to parse the return command
(def return-re #"(return)")

(defn clean-line
  "Cleans source line, removing comments and extra whitespace"
  [line]
  (-> line
      (s/split #"//")
      first
      s/trim
      (s/replace #"\s+" " ")))

(defn parse-empty-command
  "Parse an empty source line"
  [cmd-ctx parts]
  cmd-ctx)

(defn parse-arithmetic-command
  "Parse an arithmetic or logic command"
  [cmd-ctx parts]
  cmd-ctx)

(defn parse-push-pop-command
  "Parses push or pop command"
  [cmd-ctx [segment index]]
  (assoc cmd-ctx
    :segment segment
    :index (Integer/parseInt index)))

(defn parse-branching-command
  "Parses goto, if-goto or label command"
  [cmd-ctx [label]]
  (assoc cmd-ctx :label label))

(defn parse-function-command
  "Parses function command"
  [cmd-ctx [function vars]]
  (assoc cmd-ctx
    :function function
    :vars (Integer/parseInt vars)))

(defn parse-call-command
  "Parses call command"
  [cmd-ctx [function args]]
  (assoc cmd-ctx
    :function function
    :args (Integer/parseInt args)))

(defn parse-return-command
  "Parses return command"
  [cmd-ctx parts]
  cmd-ctx)

;Pairs of regexes and their corresponding parser fns
(def matchers
  [[empty-re parse-empty-command]
   [arithmetic-re parse-arithmetic-command]
   [push-pop-re parse-push-pop-command]
   [branching-re parse-branching-command]
   [function-re parse-function-command]
   [call-re parse-call-command]
   [return-re parse-return-command]])

(defn parse-if-match
  "Parses the source with the given function f if source matches
  the given regex re. f takes a command ctx map and
  a vector of args for the command"
  [source re f]
  (if-let [[source command & args] (re-matches re source)]
    (f {:source source :command command} args)
    nil))

(defn match-and-parse
  "Tests source lazily against each regex in re-f-pairs and return
  the result cmd-ctx of the first match or throw exception"
  [source re-f-pairs]
  (if-let [parsed (->> re-f-pairs
                  (map (fn [[re f]] (parse-if-match source re f)))
                  (filter identity)
                  first)]
    parsed
    (throw (Exception. (str "Cannot parse " source)))))

(defn parse-command
  "Parses a line of vm source code into a command context object based
  on the provided regex and parser fns"
  ([source]
   (parse-command source matchers))
  ([source re-f-pairs]
  (-> source
      clean-line
      (match-and-parse re-f-pairs))))
