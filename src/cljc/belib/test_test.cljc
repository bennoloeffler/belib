(ns belib.test-test
  (:require [clojure.test :as t]
            [hyperfiddle.rcf :refer [tests]]
            #?(:clj [belib.test :as bt :refer [expect-ex]]
               :cljs [belib.test :as bt :refer-macros [expect-ex]])))

(hyperfiddle.rcf/enable! false)

(defn fail-fun []
  (throw (ex-info "ERR" {})))

(comment
  (def ab 4)
  (instance? Symbol `ab)
  (instance? js/Number 12) ; does not work in js
  (type 34)
  (= js/Number (type 17.0)))

(tests
  (expect-ex (fail-fun)) := #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core.ExceptionInfo))


