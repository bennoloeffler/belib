(ns belib.core-test
  (:require [clojure.test :refer :all]
            [belib.core :refer :all]))

(deftest test-test
  (testing "test the test function"
    (is (= (test-belib) "BELs lib is working..."))))
