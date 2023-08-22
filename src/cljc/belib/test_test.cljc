(ns belib.test-test
  (:require [clojure.test :as t]
            [hyperfiddle.rcf :refer [tests]]
            #?(:clj  [belib.test :as bt :refer [expect-ex]]
               :cljs [belib.test :as bt :refer-macros [expect-ex]])))

(hyperfiddle.rcf/enable! true)

; EXAMPLE
; see date-time.cljc, line 180

(defn fail-fun []
  (throw (ex-info "ERR" {})))

(tests
  (expect-ex (fail-fun)) := #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core.ExceptionInfo))


(tests
  ; compiles in cljs but not in clj because js/Error ???
  ;(expect-ex (assert false)) := #?(:clj java.lang.AssertionError :cljs js/Error)
  (expect-ex (/ 1 0)) := #?(:clj java.lang.ArithmeticException :cljs ##Inf)
  #?(:clj (expect-ex ArithmeticException (/ 1 0)) := true))

