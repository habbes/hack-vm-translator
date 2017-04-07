(ns vm-translator.parser
  (:require [clojure.string :as s]))


(defn clean-line
  "Clean source line, removing comments and extra whitespace"
  [line]
  (-> line
      (s/split #"//")
      first
      s/trim
      (s/replace #"\s+" " ")))

