(ns vm-translator.file
  (:require [clojure.java.io :as io]
            [clojure.java.string :as s]))


(defn remove-ext
  "Removes file extension from path"
  [path]
  (s/replace path
             #"(\.\w+)$"
             ""))

(defn dir?
  "Checks whether the specified path is a directory"
  [path]
  (.isDir (io/file path)))

(defn vm-file?
  "Checks whether the specified file is a .vm file"
  [path]
  (s/ends-with path ".vm"))

(defn get-vm-files
  "Get a list of vm files in the specified directory"
  [dir-path]
  (let [dir (io/as-file dir)
        files (.listFiles dir)]
    (filter (vm-file? files))))

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
