(ns vm-translator.file-test
  (:require [clojure.test :refer :all]
            [vm-translator.file :refer :all]
            [clojure.java.io :as io]))

(deftest vm-file-test
  (testing "Accepts path with .vm extension"
    (let [path "Sample.vm"]
      (is (= (vm-file? path) true)))
    (let [path "this/is/a/File.vm"]
      (is (= (vm-file? path) true)))
    (let [path "../path/to/Class.vm"]
      (is (= (vm-file? path) true)))
    (let [path "./my/Math.vm"]
      (is (= (vm-file? path) true)))
    (let [path "/path/to/test.vm"]
      (is (= (vm-file? path)))))
  (testing "Rejects path without .vm extension"
    (is (= false (vm-file? "Sample.test")))
    (is (= false (vm-file? "/some/path")))
    (is (= false (vm-file? "../test/File.cmp")))
    (is (= false (vm-file? "/code/project/Project/README.md")))))

(deftest get-vm-files-test
  (testing "Returns set of vm files paths in directory"
    (let [paths (get-vm-files "test/test_files/NestedCall")]
      (is (= paths #{"test/test_files/NestedCall/Sys.vm"})))
    (let [paths (get-vm-files "test/test_files/FibonacciElement")]
      (is (= paths #{"test/test_files/FibonacciElement/Main.vm"
                     "test/test_files/FibonacciElement/Sys.vm"})))))

(deftest dir?-test
  (testing "Accepts directories"
    (is (= true (dir? "test/test_files/empty_dir")))
    (is (= true (dir? "test/test_files/NestedCall")))
    (is (= true (dir? "test/test_files/FibonacciElement"))))
  (testing "Rejects files"
    (is (= false (dir? "test/test_files/empty_file")))
    (is (= false (dir? "test/test_files/SimpleAdd.vm")))
    (is (= false (dir? "test/test_files/SimpleAdd.asm")))))

(deftest get-output-path-test
  (testing "Replace filename extension with .asm"
    (let [input "Sample.vm" out (get-output-path input)]
      (is (= out "Sample.asm"))))
  (testing "Should support absolute paths"
    (let [input "/this/is/a/Program.vm"
          out (get-output-path input)]
      (is (= out "/this/is/a/Program.asm"))))
  (testing "Should support relative paths"
    (let [input "path/to/File.vm"
          out (get-output-path input)]
      (is (= out "path/to/File.asm")))
    (let [input "../path/to/file.vm"
          out (get-output-path input)]
      (is (= out "../path/to/file.asm")))
    (let [input "./path/to/file.vm"
          out (get-output-path input)]
      (is (= out "./path/to/file.asm"))))
  (testing "For directories, appends a file with same name in the directory"
    (let [input "test/FibonacciElement"
          out (get-output-path input)]
      (is (= out "test/FibonacciElement/FibonacciElement.asm")))
    (let [input (.getAbsolutePath (io/as-file "test/NestedCall"))
          out (get-output-path input)]
      (is (= out (.getAbsolutePath (io/as-file "test/NestedCall/NestedCall.asm"))))
    (let [input "test/FibonacciElement/"
          out (get-output-path input)]
      (is (= out "test/FibonacciElement/FibonacciElement.asm"))))))

(deftest get-class-name-test
  (testing "Get filename without extension"
    (let [input "Sample.vm" out (get-class-name input)]
      (is (= out "Sample"))))
  (testing "Should support absolute paths"
    (let [input "/this/is/a/Program.vm"
          out (get-class-name input)]
      (is (= out "Program"))))
  (testing "Should support relative paths"
    (let [input "path/to/File.vm"
          out (get-class-name input)]
      (is (= out "File")))
    (let [input "../path/to/file.vm"
          out (get-class-name input)]
      (is (= out "file")))
    (let [input "./path/to/file.vm"
          out (get-class-name input)]
      (is (= out "file")))))
