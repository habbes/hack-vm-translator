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
