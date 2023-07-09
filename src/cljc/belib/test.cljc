(ns belib.test
  #?(:clj
     (:require [net.cgrand.macrovich :as macros])
     :cljs
     (:require-macros [net.cgrand.macrovich :as macros]
                      #_[belib.test :refer [expect-ex]]))

  #?(:clj (:import [java.lang Throwable])))


(defmacro expect-ex
  "check for exceptions in rcf tests:

  :clj
  (tests
    (test-ex (/ 1 0)) := ArithmeticException)

  :cljs
  (tests
    (test-ex (/ 1 0)) := js/XXX )

  You need:
   [com.hyperfiddle/rcf \"20220405\"] in project.clj
   [hyperfiddle.rcf :refer [tests]] required and
   (hyperfiddle.rcf/enable! true)"
  ([expr]
   (if (:ns &env) ;; :ns only exists in CLJS
     `(try ~expr
           (catch js/Error e# (type e#)))
     `(try ~expr
           (catch Throwable e# (type e#)))))
  ([exception-type expr]
   (if (:ns &env) ;; :ns only exists in CLJS
     `(try (do
             ~expr
             false)
           (catch Throwable e#
             (instance? ~exception-type e#)))
     `(try (do
             ~expr
             false)
           (catch js/Error e#
             (instance? ~exception-type e#))))))
