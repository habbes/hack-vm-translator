(ns vm-translator.context-test
  (:require [clojure.test :refer :all]
            [vm-translator.context :refer :all]))

(deftest initialize-test
  (testing "Initializes new context map"
    (let [ctx (initialize)]
      (is (= ctx {:line-number 1
                  :instruction-number 0})))))

(deftest inc-line-test
  (testing "Increments line number by 1"
    (let [ctx {:line-number 6
               :instruction-number 4}
          new-ctx (inc-line ctx)]
      (is (= new-ctx {:line-number 7
                      :instruction-number 4})))))

(deftest inc-instruction-test
  (testing "Inscrements instruction by 1 by default"
    (let [ctx {:line-number 6
               :instruction-number 4}
          new-ctx (inc-instruction ctx)]
      (is (= new-ctx {:line-number 6
                      :instruction-number 5}))))

  (testing "Increments instruction by specified number"
    (let [ctx {:line-number 6
               :instruction-number 4}
          new-ctx (inc-instruction ctx 12)]
      (is (= new-ctx {:line-number 6
                      :instruction-number 16})))))
