(ns vm-translator.file
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))


(defn- remove-ext
  "Removes file extension from path"
  [path]
  (s/replace path
             #"(\.\w+)$"
             ""))

(defn vm-file?
  "Checks whether the specified file is a .vm file"
  [path]
  (s/ends-with? path ".vm"))

(defn dir?
  "Checks whether the specified path is a directory"
  [path]
  (.isDirectory (io/file path)))

(defn get-vm-files
  "Returns a set of vm files in the specified directory"
  [path]
  (let [dir (io/as-file path) files (.listFiles dir)]
    (->> files
         (map #(.getPath %))
         (filter vm-file?)
         (into #{}))))

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
