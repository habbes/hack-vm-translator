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
  (let [[out ctx]
        (-> line
            parser/parse-command
            (assoc :context ctx)
            code/translate-with-comment)]
    [out (context/inc-line ctx)]))

(defn translate-lines
  "Translates each line in the lines seq and pass each output
  to the output handler fn"
  [lines output-handler ctx]
  (loop [n 0 ctx ctx]
    (if-let [line (nth lines n nil)]
      (let [[out new-ctx] (translate-line line ctx)]
        (recur (inc n)
               (do (if out (output-handler out))
                 new-ctx)))
    ctx)))

(defn create-writer-output-handler
  "Returns an output-handler which writes ouput
  to the specified wrtr"
  [wrtr]
  (fn [out]
   (.write wrtr
           (str out "\n"))))

(defn translate-source
  "Reads vm source code from rdr and writes the output
  assembly code into wrtr"
  [rdr wrtr class-name]
  (let [lines (line-seq rdr)
        handler (create-writer-output-handler wrtr)
        ctx (context/initialize class-name)]
    (translate-lines lines handler ctx)))

(defn get-output-path
  "Get the output path for the output file based on
  the input vm file."
  [input-path]
  (s/replace input-path
             #"([a-zA-Z0-9_\- ]+)\.vm$"
             "$1.asm"))

(defn get-class-name
  "Gets the class name based on the input vm file"
  [input-path]
  (nth
    (re-find #"([a-zA-Z0-9_\-]+)\.vm$" input-path)
    1))

(defn translate-file
  "Translate the input vm file and store the asm output in
  the specified output file"
  [input-path output-path class-name]
  (with-open [rdr (io/reader input-path)]
    (with-open [wrtr (io/writer output-path)]
      (prn "Translating to" output-path)
      (translate-source rdr wrtr class-name)
      (prn "Operation complete"))))

(defn -main
  "I don't do a whole lot ... yet."
  [input-path]
  (let [output-path (get-output-path input-path)
        class-name (get-class-name input-path)]
    (translate-file input-path output-path class-name)))
