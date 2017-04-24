(ns vm-translator.core
  (:require [vm-translator.parser :as parser]
            [vm-translator.code :as code]
            [vm-translator.context :as context]
            [vm-translator.file :as file]
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

(defn make-line-reducer
  "Returns a fn which when supplied a ctx and line will translate the line,
  feed it to the specified handler and return the updated ctx"
  [handler ctx]
  (fn [ctx line]
    (let [[out new-ctx] (translate-line line ctx)]
      (if out (handler out))
      new-ctx)))

(defn translate-lines
  "Translates each line in the lines seq and pass each output
  to the output handler fn"
  [lines output-handler ctx]
  (let [reducer (make-line-reducer output-handler ctx)]
    (reduce reducer ctx lines)))

(defn make-writer-output-handler
  "Returns an output-handler which writes ouput
  to the specified wrtr"
  [wrtr]
  (fn [out]
   (.write wrtr
           (str out "\n"))))

(defn translate-source
  "Reads vm source code from rdr and writes the output
  assembly code into wrtr"
  [rdr wrtr ctx]
  (let [lines (line-seq rdr)
        handler (make-writer-output-handler wrtr)]
    (translate-lines lines handler ctx)))

(defn translate-file
  "Translate the input vm file and store the asm output in
  the specified output file"
  [input-path output-path]
  (let [cls (file/get-class-name input-path)
        ctx (context/initialize cls)]
    (with-open [rdr (io/reader input-path)
                wrtr (io/writer output-path)]
        (translate-source rdr wrtr ctx))))


(defn translate-file-to-writer
  "Translates the input file and write the output
  in the specified writer"
  [input-path wrtr ctx]
  (let [cls (file/get-class-name input-path)
        ctx (context/set-class ctx cls)]
    (with-open [rdr (io/reader input-path)]
      (translate-source rdr wrtr ctx))))

(defn make-files-reducer
  "Returns a reducer fn that given a ctx and input path
  will translate the input into the specified wrtr and return
  the updated ctx"
  [wrtr]
  (fn [ctx path]
    (translate-file-to-writer path wrtr ctx)))

(defn translate-files
  "Translates a vector of input vm files into the specified
  asm output file"
  [input-paths output-path]
  (let [ctx (context/initialize)]
    (with-open [wrtr (io/writer output-path)]
      (reduce (make-files-reducer wrtr)
              ctx input-paths))))

(defn translate-dir
  "Translate a dir's vm files in to the specified asm
  output file"
  [dir output-file]
  (let [input-files (file/get-vm-files dir)]
    (translate-files input-files output-file)))

(defn find-translator
  "Returns the suitable translator fn for the given
  input path depending on whether it refers to a file or dir"
  [path]
  (if (file/dir? path)
    translate-dir
    translate-file))

(defn -main
  "Translates a hack vm source file or directory into a
  hack asm output file"
  [input-path]
  (let [output-path (file/get-output-path input-path)
        translate (find-translator input-path)]
    (println "Translating into" output-path)
    (translate input-path output-path)
    (println "Operation complete")))
