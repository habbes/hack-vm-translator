(ns vm-translator.core
  (:require [vm-translator.parser :as parser]
            [vm-translator.code :as code]
            [clojure.string :as s]
            [clojure.java.io :as io])
  (:gen-class))

(defn translate-line
  "Translates line of vm source code to hack assembly code.
  Returns nil if source code is invalid."
  [line]
  (if-let [cmd (parser/parse-command line)]
    (code/translate-with-comment cmd)
    nil))

(defn translate-source
  "Reads vm source code from rdr and writes the output
  assembly code into wrtr"
  [rdr wrtr]
  (let [lines (line-seq rdr)]
    (loop [count 0]
      (if-let [line (nth lines count nil)]
        (if-let [out (translate-line line)]
          (do
            (.write wrtr out)
            (recur (inc count)))
          nil)
        nil))))

(defn get-output-path
  "Get the output path for the output file based on
  the input vm file."
  [input-path]
  (s/replace input-path
             #"([a-zA-Z0-9_\- ]+)\.vm$"
             "$1.asm"))

(defn translate-file
  "Translate the input vm file and store the asm output in
  the specified output file"
  [input-path output-path]
  (with-open [rdr (io/reader input-path)]
    (with-open [wrtr (io/writer output-path)]
      (prn "Translating to" output-path)
      (translate-source rdr wrtr)
      (prn "Operation complete"))))

(defn -main
  "I don't do a whole lot ... yet."
  [input-path]
  (let [output-path (get-output-path input-path)]
    (translate-file input-path output-path)))
