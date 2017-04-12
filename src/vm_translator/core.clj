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
    (str
      (code/translate-with-comment cmd)
      "\n")
    nil))

(defn translate-lines
  "Translates each line in the lines seq and pass each output
  to the output handler fn"
  [lines output-handler]
  (loop [count 0]
    (if-let [line (nth lines count nil)]
      (let [out (translate-line line)]
        (output-handler out)
        (recur (inc count))))))

(defn create-writer-output-handler
  "Returns an output-handler which writes ouput
  to the specified wrtr"
  [wrtr]
  (fn [out]
    (if-let [out out] (.write wrtr out))))

(defn translate-source
  "Reads vm source code from rdr and writes the output
  assembly code into wrtr"
  [rdr wrtr]
  (let [lines (line-seq rdr)
        handler (create-writer-output-handler wrtr)]
    (translate-lines lines handler)))

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
