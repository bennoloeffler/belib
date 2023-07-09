(ns belib.core-test
  (:require [clojure.test :refer :all]
            [belib.core :refer :all]
            [belib.spec :refer :all]
            [belib.cal-week-year :refer :all]
            [belib.test-test :refer :all]))

(deftest test-test
  (testing "test the core namespace and the test function"
    (is (= (test-belib) "BELs lib seems to work.\n"))))

(deftest test-spec
  (testing "test the belib-spec namespace"
    (is (email? "abc@def.com"))))

(deftest test-cw
  (testing "test the belib.cal-week-year namespace"
    (is (get-abs-current-week))))

(comment
  (run-tests))