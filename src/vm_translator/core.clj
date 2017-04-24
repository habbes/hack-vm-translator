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

(defn inject-bootstrap
  "Injects bootstrapping code to init the asm program to
  the specified output handler"
  [handler ctx]
  (let [cmd {:source "init" :command "init"
             :context ctx}
        [code new-ctx] (code/translate cmd)]
    (handler code)
    new-ctx))

(defn translate-source
  "Reads vm source code from rdr and writes the output
  assembly code into wrtr"
  [rdr handler ctx]
  (let [lines (line-seq rdr)]
    (translate-lines lines handler ctx)))

(defn translate-file
  "Translates the input vm file and store and feed the
  asm code to the output handler"
  [input-path handler ctx]
  (let [cls (file/get-class-name input-path)
        ctx (context/set-class ctx cls)]
    (with-open [rdr (io/reader input-path)]
        (translate-source rdr handler ctx))))

(defn make-files-reducer
  "Returns a reducer fn that given a ctx and input path
  will translate the input into the specified wrtr and return
  the updated ctx"
  [handler]
  (fn [ctx path]
    (translate-file path handler ctx)))

(defn translate-files
  "Translates a vector of input vm files into the specified
  asm output file"
  [input-paths handler ctx]
  (reduce (make-files-reducer handler)
          ctx input-paths))

(defn translate-dir
  "Translates a dir's vm files in to the specified asm
  output handler"
  [dir handler ctx]
  (let [input-files (file/get-vm-files dir)]
    (translate-files input-files handler ctx)))

(defn find-translator
  "Returns the suitable translator fn for the given
  input path depending on whether it refers to a file or dir"
  [path]
  (if (file/dir? path)
    translate-dir
    translate-file))

(defn translate
  "Translate the vm source at the input path and feed the
  output to the specified handler. If :boot is true,
  bootstrapping code is injected at the top of the output."
  [input-path handler & {boot :boot :or [boot false]}]
    (let [f (find-translator input-path)
          ctx (context/initialize)
          init-ctx (if boot (inject-bootstrap ctx) ctx)]
      (f input-path handler init-ctx)))

(defn -main
  "Translates a hack vm source file or directory into a
  hack asm output file"
  [input-path]
  (let [output-path (file/get-output-path input-path)]
    (println "Translating into" output-path)
    (with-open [wrtr (io/writer output-path)]
      (let [handler (make-writer-output-handler wrtr)]
        (translate input-path handler :boot true)))
    (println "Operation complete")))
