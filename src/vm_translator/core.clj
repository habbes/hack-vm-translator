(ns vm-translator.core
  (:require [vm-translator.parser :as parser]
            [vm-translator.code :as code]
            [vm-translator.context :as context]
            [clojure.string :as s]
            [clojure.java.io :as io])
  (:gen-class))

(defn- remove-ext
  "Removes file extension from path"
  [path]
  (s/replace path
             #"(\.\w+)$"
             ""))

(defn is-vm-file?
  "Checks whether the specified file is a .vm file"
  [path]
  (s/ends-with path ".vm"))

(defn get-vm-files
  "Get a list of vm files in the specified directory"
  [dir-path]
  (let [dir (io/as-file dir)
        files (.listFiles dir)]
    (filter (is-vm-file? files))))

(defn get-output-path
  "Gets the output path for the output file based on
  the input vm file."
  [input-path]
  (-> input-path
      (io/as-file)
      (.getPath)
      remove-ext
      (str ".asm")))

(defn get-class-name
  "Gets the class name based on the input vm file"
  [input-path]
  (-> (.getName (io/file input-path))
      (remove-ext)))

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

(defn translate-file
  "Translate the input vm file and store the asm output in
  the specified output file"
  [input-path output-path]
  (let [cls (get-class-name input-path)
        ctx (context/initialize cls)]
    (with-open [rdr (io/reader input-path)]
      (with-open [wrtr (io/writer output-path)]
        (translate-source rdr wrtr class-name)))))


(defn translate-file-to-writer
  "Translates the input file and write the output
  in the specified writer"
  [input-path wrtr ctx]
  (let [cls (get-class-name input-path)
        ctx (context/set-class ctx cls)]
    (with-open [rdr (io/reader input-path)]
      (translate-source rdr wrtr ctx))))

(defn create-files-reducer
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
      (reduce (create-files-reducer wrtr)
              input-paths ctx))))

(defn translate-dir
  "Translate a dir's vm files in to the specified asm
  output file"
  [dir output-file]
  (let [input-files (get-vm-files dir)]
    (translate-files input-files output-file)))

(defn find-translator
  "Returns the suitable translator fn for the given
  input path depending on whether it refers to a file or dir"
  [path]
  (if (.isDir (io/as-file path))
    translate-dir
    translate-file))

(defn -main
  "Translates a hack vm source file or directory into a
  hack asm output file"
  [input-path]
  (let [output-path (get-output-path input-path)
        translate (find-translator input-path)]
    (println "Translating into" output-path)
    (translate input-path output-path)
    (println "Operation complete")))
