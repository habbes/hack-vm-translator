(ns vm-translator.parser
  (:require [clojure.string :as s]))


; regex used to parse arithmetic and logic commands
(def arithmetic-re #"(add|sub|neg|eq|gt|lt|and|or|not)")
; regex used to parse push and pop commands
(def push-pop-re #"(push|pop) (arg|local|this|that|constant|static|temp|pointer) (\d+)")

(declare parse-arithmetic-command)
(declare parse-push-pop-command)

;Pairs of regexes and their corresponding parser fns
(def re-parser-pairs
  [[arithmetic-re parse-arithmetic-command]
   [push-pop-re parse-push-pop-command]])

(defn clean-line
  "Cleans source line, removing comments and extra whitespace"
  [line]
  (-> line
      (s/split #"//")
      first
      s/trim
      (s/replace #"\s+" " ")))

(defn parse-arithmetic-command
  "Parse an arithmetic or logic command"
  [cmd-ctx parts]
  cmd-ctx)

(defn parse-push-pop-command
  "Parses push or pop command"
  [cmd-ctx [segment index]]
  (assoc cmd-ctx :segment segment :index index))

(defn parse-if-match
  "Parses the source with the given function f if source matches
  the given regex re"
  [source re f]
  (if-let [[source command & parts] (re-matches re source)
           cmd-obj {:source source :command command}]
    (fn cmd-ctx parts)
    nil))

(defn match-and-parse
  "Tests source lazily against each regex in re-parser-pairs and return
  the result cmd-ctx of the first match or nil"
  [source re-f-pairs]
  (first
    (map
      (fn [[re f]] (parse-if-match source re f))
      re-f-pairs)))

(defn parse-command
  "Parses a line of vm source code into a command context object based
  on the provided regex and parser fns"
  ([source]
   (parse-command re-parser-pairs))
  ([source re-f-pairs]
  (-> source
      clean-line
      (match-and-parse re-f-pairs))))
