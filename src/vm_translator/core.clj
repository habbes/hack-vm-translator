(ns vm-translator.core
  (:require [vm-translator.parser :as parser]
            [vm-translator.code :as code]
            [vm-translator.context :as context]
            [clojure.string :as s]
            [clojure.java.io :as io])
  (:gen-class))

(defn translate-line
  "Translates line of vm source code to hack assembly code.
  Returns nil if source code is invalid."
  [line ctx]
  (if-let [cmd (parser/parse-command line)]
    (str
      (code/translate-with-comment (conj cmd [:context ctx]))
      "\n")
    nil))

(defn translate-lines
  "Translates each line in the lines seq and pass each output
  to the output handler fn"
  [lines output-handler ctx]
  (loop [n 0 ctx ctx]
    (if-let [line (nth lines n nil)]
      (let [out (translate-line line ctx)
            ;TODO this is really messy code, please cleanup
            inst-count (max 0 (- (if-let [out out] (count (s/split-lines out)) 0) 1))
            new-ctx (-> ctx
                        context/inc-line
                        (context/inc-instruction inst-count))]
        (output-handler out)
        (recur (inc n) new-ctx)))))

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
        handler (create-writer-output-handler wrtr)
        ctx (context/initialize)]
    (translate-lines lines handler ctx)))

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
