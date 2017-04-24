(ns vm-translator.file-test
  (:require [clojure.test :refer :all]
            [vm-translator.file :refer :all]))

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
  (testing "Should support directories"
    (let [input "path/to/MyProject"
          out (get-output-path input)]
      (is (= out "path/to/MyProject.asm")))
    (let [input "/path/to/Project"
          out (get-output-path input)]
      (is (= out "/path/to/Project.asm")))
    (let [input "path/to/App/"
          out (get-output-path input)]
      (is (= out "path/to/App.asm")))))

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
