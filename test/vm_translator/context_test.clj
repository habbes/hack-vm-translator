(ns vm-translator.context-test
  (:require [clojure.test :refer :all]
            [vm-translator.context :refer :all]))

(deftest initialize-test
  (testing "Initializes new context map with given class name"
    (let [ctx (initialize "MyClass")]
      (is (= ctx {:line-number 0
                  :instruction-number -1
                  :class "MyClass"})))))

(deftest inc-line-test
  (testing "Increments line number by 1"
    (let [ctx {:line-number 6
               :instruction-number 4
               :class "MyClass"}
          new-ctx (inc-line ctx)]
      (is (= new-ctx {:line-number 7
                      :instruction-number 4
                      :class "MyClass"})))))

(deftest inc-instruction-test
  (testing "Inscrements instruction by 1 by default"
    (let [ctx {:line-number 6
               :instruction-number 4
               :class "Sample"}
          new-ctx (inc-instruction ctx)]
      (is (= new-ctx {:line-number 6
                      :instruction-number 5
                      :class "Sample"}))))

  (testing "Increments instruction by specified number"
    (let [ctx {:line-number 6
               :instruction-number 4
               :class "Class"}
          new-ctx (inc-instruction ctx 12)]
      (is (= new-ctx {:line-number 6
                      :instruction-number 16
                      :class "Class"})))))

(deftest set-class-test
  (testing "Adds class key to context"
    (let [ctx {:line-number 1
               :instruction-number 0}
          new-ctx (set-class ctx "SomeClass")]
      (is (= new-ctx {:line-number 1
                      :instruction-number 0
                      :class "SomeClass"}))))
  (testing "Replaces existing class"
    (let [ctx {:line-number 1
               :instruction-number 0
               :class "Sample"}
          new-ctx (set-class ctx "Other")]
      (is (= new-ctx {:line-number 1
                      :instruction-number 0
                      :class "Other"})))))

(deftest unset-class-test
  (testing "Removes class key from context"
    (let [ctx {:line-number 1
               :instruction-number 0
               :class "Math"}
          new-ctx (unset-class ctx)]
      (is (= new-ctx {:line-number 1
                      :instruction-number 0}))))
  (testing "Does nothing if class key was not set"
    (let [ctx {:line-number 1
               :instruction-number 0}
          new-ctx (unset-class ctx)]
      (is (= new-ctx ctx)))))

(deftest set-function-test
  (testing "Adds function key to the context"
    (let [ctx {:line-number 1
               :instruction-number 0
               :class "MyClass"}
          new-ctx (set-function ctx "MyClass.func")]
      (is (= new-ctx {:line-number 1
                      :instruction-number 0
                      :class "MyClass"
                      :function "MyClass.func"}))))
  (testing "Replaces existing function"
    (let [ctx {:line-number 1
               :instruction-number 0
               :class "MyClass"
               :function "MyClass.func"}
          new-ctx (set-function ctx "MyClass.newFunc")]
      (is (= new-ctx {:line-number 1
                      :instruction-number 0
                      :class "MyClass"
                      :function "MyClass.newFunc"})))))

(deftest unset-function-test
  (testing "Removes function key from context"
    (let [ctx {:line-number 1
               :instruction-number 0
               :function "mult"}
          new-ctx (unset-function ctx)]
      (is (= new-ctx {:line-number 1
                      :instruction-number 0}))))
  (testing "Does nothing if function key was not set"
    (let [ctx {:line-number 1
               :instruction-number 0}
          new-ctx (unset-function ctx)]
      (is (= new-ctx ctx)))))
