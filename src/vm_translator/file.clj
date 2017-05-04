(ns vm-translator.file
  (:require [clojure.java.io :as io]
            [clojure.string :as s])
  (:import [java.nio.file Paths]))


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

(defn- get-output-path-for-file
  "Gets the output path for the output asm file based on
  the input vm file's path"
  [input-path]
  (-> input-path
      (io/as-file)
      (.getPath)
      remove-ext
      (str ".asm")))

(defn- get-output-path-for-dir
  "Gets the output path for the output asm file
  based on the input vm dir's path"
  [input-path]
  (let [dir (io/as-file input-path)
        name (.getName dir)]
    (-> input-path
        (Paths/get (str name ".asm"))
        (.toString))))

(defn get-output-path
  "Gets the output path for the output file based on
  the input vm file or dir."
  [input-path]
  (if (dir? input-path)
    (get-output-path-for-dir input-path)
    (get-output-path-for-file input-path)))

(defn get-class-name
  "Gets the class name based on the input vm file"
  [input-path]
  (-> (.getName (io/file input-path))
      (remove-ext)))
