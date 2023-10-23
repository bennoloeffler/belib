(ns belib.test-test
  (:require [clojure.test :as t]
            [borkdude.deflet :refer [deflet]]
            [hyperfiddle.rcf :refer [tests]]
            #?(:clj  [belib.test :as bt :refer [expect-ex return-ex return:error-if-ex]]
               :cljs [belib.test :as bt :refer-macros [expect-ex return-ex return:error-if-ex]])))

(hyperfiddle.rcf/enable! false)

; EXAMPLE
; see date-time.cljc, line 180

(tests

  "test exceptions"

  ;; hyperfiddle :throws does not work!
  ;;(assert false "boom") :throws #?(:clj java.lang.AssertionError :cljs js/Error)

  ;; this does work!
  (expect-ex (assert false "boom")) := #?(:clj java.lang.AssertionError :cljs js/Error)

  :end-test)


(defn fail-fun []
  (throw (ex-info "ERR" {:data "some"})))

(tests
  (deflet
    ;; use def as let in order to use it for working with repl
    (def err (-> (fail-fun)
                 return-ex
                 ex-message))
    err := "ERR"))




(tests

  ; DOES NOT WORK
  ;(assert (fail-fun)) :throws clojure.lang.ExceptionInfo
  ;(fail-fun) :throws cljs.core.ExceptionInfo
  ;(assert false "boom") :throws java.lang.AssertionError

  (expect-ex (fail-fun)) := #?(:clj  clojure.lang.ExceptionInfo
                               :cljs cljs.core.ExceptionInfo)


  (expect-ex (assert false)) := #?(:clj  java.lang.AssertionError
                                   :cljs js/Error)
  (expect-ex (/ 1 0)) := #?(:clj  java.lang.ArithmeticException
                            :cljs ##Inf)

  #?(:clj (expect-ex ArithmeticException (/ 1 0)) := true))



(tests
  "handle details of errors"
  (ex-data (return-ex (fail-fun))) := {:data "some"}
  (ex-message (return-ex (fail-fun))) := "ERR"

  (ex-message (return-ex (assert (= 2 1))))
  := "Assert failed: (= 2 1)"

  (ex-message (return-ex (assert (= 2 1) "should be equal")))
  := "Assert failed: should be equal\n(= 2 1)"

  #?(:cljs (ex-message (return-ex (throw (js/Error. "something")))) := "something"
     :clj  (ex-message (return-ex (throw (RuntimeException. "something else")))) := "something else"))


(tests
  "unify tests, if exceptions are different in cljs and clj - eg when parsing dates"
  (return:error-if-ex (fail-fun)) := :error)